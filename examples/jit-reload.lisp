(defpackage #:zk-llvm/examples/jit-reload
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm)
                    (#:ffi #:zk-ffi))
  (:export #:main))

(in-package #:zk-llvm/examples/jit-reload)

(defun zone-rate (zone)
  (ecase zone
    (1 3)
    (2 5)))

(defun shipping-plugin (jit surcharge)
  (llvm:define-module (module "shipping" :jit jit)
    (llvm:declare-ir zone-rate (:i64) :i64)
    (llvm:defir quote-shipping ((weight :i64) (zone :i64)) :i64
      (llvm:+ (llvm:* weight (llvm:call 'zone-rate zone)) surcharge))))

(defun report-quote (stage quote expected)
  (let ((amount (funcall quote 4 1)))
    (assert (= amount expected))
    (format t "~A shipping quote for 4 kg in zone 1: ~D~%" stage amount)))

(defun reload-shipping-plugin (original candidate)
  ;; The replacement stays loaded while the original drains and unloads.
  (llvm:with-jit-module (candidate (shipping-plugin candidate 5))
    (llvm:with-jit-binding (quote-v2 candidate 'quote-shipping :int64 :int64 :int64)
      (let (active-quote)
        (llvm:with-jit-module (original (shipping-plugin original 0))
          (llvm:with-jit-binding (quote-v1 original 'quote-shipping :int64 :int64 :int64)
            (setf active-quote quote-v1)
            (report-quote "Active v1" active-quote 12)
            (report-quote "Candidate v2" quote-v2 17)
            (setf active-quote quote-v2)
            ;; A request holding the old binding can still finish after the switch.
            (report-quote "Draining v1" quote-v1 12)))
        (assert (handler-case
                    (llvm:with-jit-binding (retired original 'quote-shipping :int64 :int64 :int64)
                      nil)
                  (llvm:llvm-error (condition)
                    (eq (llvm:llvm-error-operation condition) 'llvm:jit-lookup))))
        (report-quote "Active v2" active-quote 17)))))

(defun main ()
  (llvm:with-objects ()
    (llvm:with-jit (host)
      (llvm:jit-define-symbol host "zone_rate"
        (ffi:make-callback #'zone-rate :int64 :int64))
      (llvm:with-jit (original (llvm:make-jit-dylib host "shipping-v1"))
        (llvm:with-jit (candidate (llvm:make-jit-dylib host "shipping-v2"))
          (reload-shipping-plugin original candidate))))))
