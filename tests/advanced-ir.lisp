(in-package #:zk-llvm/tests)

(defun test-call-bundles-and-assembly ()
  (llvm:with-objects ()
    (let* ((module
             (llvm:define-module (module "call-bundles")
               (llvm:declare-ir marker (:i32) :i32)
               (llvm:defir caller ((input :i32)) :i32
                 (llvm:with-operand-bundles ((llvm:operand-bundle "deopt" input))
                   (llvm:call 'marker input)))))
           (caller (llvm:find-function module "caller"))
           (call (first (llvm:block-instructions (first (llvm:function-blocks caller)))))
           (bundle (first (llvm:call-operand-bundles call))))
      (assert (string= "deopt" (llvm:operand-bundle-tag bundle)))
      (assert (search "\"deopt\"" (princ-to-string bundle)))
      (assert (llvm:value-equal-p (llvm:parameter caller 0)
                                 (first (llvm:operand-bundle-operands bundle))))
      (assert (= 1 (length (llvm:call-arguments call))))
      (let* ((octets (make-array 3 :element-type '(unsigned-byte 8)
                                 :initial-contents '(0 255 65)))
             (binary (llvm:operand-bundle octets)))
        (assert (equalp octets (llvm:operand-bundle-tag binary)))))
    (let* ((module
             (llvm:define-module (module "inline-assembly")
               (let ((assembly (llvm:inline-asm '(:fn :i32 (:i32)) "" "=r,0")))
                 (llvm:defir identity ((input :i32)) :i32
                   (llvm:indirect-call '(:fn :i32 (:i32)) assembly input)))))
           (function (llvm:find-function module "identity"))
           (call (first (llvm:block-instructions (first (llvm:function-blocks function)))))
           (info (llvm:inline-asm-info (llvm:call-target call))))
      (assert (string= "" (getf info :assembly)))
      (assert (string= "=r,0" (getf info :constraints)))
      (assert (eq :att (getf info :dialect)))
      (assert (null (llvm:inline-asm-info (llvm:parameter function 0))))
      (assert (llvm:verify-module module)))))

(defun test-pass-options-and-flag-edits ()
  (llvm:with-objects ()
    (let* ((context (llvm:make-context))
           (module (llvm:parse-ir context
                     "define double @math(double %x) {
                        %sum = fadd fast double %x, 0.0
                        ret double %sum
                      }
                      define i32 @first(i32 %x) { %sum = add i32 %x, 0 ret i32 %sum }
                      define i32 @second(i32 %x) { %sum = add i32 %x, 0 ret i32 %sum }"))
           (math (llvm:find-function module "math"))
           (sum (llvm:find-instruction math "sum"))
           (edited (llvm:with-module-edit (edit module)
                     (llvm:edit-set-fast-math-flags edit sum :nnan)
                     (llvm:edit-set-fast-math-flags edit sum nil)))
           (first (llvm:find-function module "first"))
           (options (llvm:make-pass-options :verify-each t :loop-unrolling nil
                                            :inliner-threshold -1))
           (optimized (llvm:optimize-function first "instcombine" :options options)))
      (assert (equal '(:fast) (llvm:instruction-fast-math-flags sum)))
      (assert (equal '(nil t)
                     (multiple-value-list
                      (llvm:instruction-fast-math-flags
                       (llvm:find-instruction (llvm:find-function edited "math") "sum")))))
      (assert (null (llvm:find-instruction (llvm:find-function optimized "first") "sum")))
      (assert (llvm:find-instruction (llvm:find-function optimized "second") "sum"))
      (assert (llvm:find-instruction first "sum"))
      (assert (llvm:verify-module (llvm:optimize-module module :o1 :options options)))
      (let* ((module (llvm:parse-ir context
                       "define i32 @memory(ptr %p, i32 %x) {
                          %next = add nsw i32 %x, 1
                          %address = getelementptr inbounds i32, ptr %p, i64 0
                          %load = load atomic volatile i32, ptr %address acquire, align 4
                          %old = atomicrmw add ptr %p, i32 1 monotonic, align 4
                          %pair = cmpxchg ptr %p, i32 1, i32 2 acq_rel acquire, align 4
                          ret i32 %old
                        }"))
             (function (llvm:find-function module "memory"))
             (sum (llvm:find-instruction function "next"))
             (gep (llvm:find-instruction function "address"))
             (load (llvm:find-instruction function "load"))
             (rmw (llvm:find-instruction function "old"))
             (pair (llvm:find-instruction function "pair")))
        (llvm:set-instruction-int-flags sum :nuw)
        (assert (equal '(:nuw) (llvm:instruction-int-flags sum)))
        (llvm:set-gep-flags gep nil)
        (assert (equal '(nil t) (multiple-value-list (llvm:gep-flags gep))))
        (llvm:set-instruction-memory-ordering load :seq-cst)
        (llvm:set-instruction-atomic-sync-scope load :single-thread)
        (llvm:set-instruction-volatile load nil)
        (assert (eq :seq-cst (llvm:instruction-memory-ordering load)))
        (assert (eq :single-thread (llvm:instruction-atomic-sync-scope load)))
        (assert (not (llvm:instruction-volatile-p load)))
        (llvm:set-instruction-atomic-rmw-operation rmw :sub)
        (assert (eq :sub (llvm:instruction-atomic-rmw-operation rmw)))
        (llvm:set-instruction-cmpxchg-orderings pair :seq-cst :acquire)
        (llvm:set-instruction-cmpxchg-weak pair t)
        (assert (llvm:instruction-cmpxchg-weak-p pair))
        (assert (eq :seq-cst (llvm:instruction-cmpxchg-success-ordering pair)))
        (dolist (instruction (list rmw pair))
          (llvm:set-instruction-alignment instruction 8)
          (assert (= 8 (llvm:instruction-alignment instruction))))
        (assert (llvm:verify-module module))))))

(defun test-computed-branches ()
  (llvm:with-objects ()
    (let ((module
            (llvm:define-module (module "computed-branches")
              (llvm:defir computed () :void
                (let* ((destination (llvm:append-block llvm:*function* "destination"))
                       (address (llvm:block-address destination)))
                  (assert (llvm:block-equal-p destination (llvm:block-address-target address)))
                  (llvm:indirect-br address (list destination))
                  (llvm:position-at-end llvm:*builder* destination)))
              (llvm:defir asm-goto () :void
                (let ((normal (llvm:append-block llvm:*function* "normal"))
                      (indirect (llvm:append-block llvm:*function* "indirect"))
                      (assembly (llvm:inline-asm '(:fn :void ()) "" "!i" :side-effects t)))
                  (llvm:call-br assembly normal (list indirect))
                  (llvm:position-at-end llvm:*builder* normal)
                  (llvm:build-ret llvm:*builder*)
                  (llvm:position-at-end llvm:*builder* indirect))))))
      (let* ((function (llvm:find-function module "asm_goto"))
             (entry (first (llvm:function-blocks function)))
             (branch (llvm:block-terminator entry)))
        (assert (eq :callbr (llvm:instruction-opcode branch)))
        (assert (equal '("normal" "indirect")
                       (mapcar #'llvm:block-name (llvm:instruction-successors branch))))
        (assert (getf (llvm:inline-asm-info (llvm:call-target branch)) :side-effects)))
      (assert (llvm:verify-module module)))))

(defun test-debug-scope-queries ()
  (llvm:with-objects ()
    (let* ((context (llvm:make-context))
           (module (llvm:make-module context "debug-queries"))
           (function (llvm:declare-function module "function" :i32 '(:i32)))
           (info (llvm:make-debug-info module "source.lisp" :language :c
                                       :flags "-g" :runtime-version 1
                                       :debug-info-for-profiling t))
           (scope (llvm:attach-function-debug-info info function 3))
           (location (llvm:make-debug-location scope 4 2)))
      (assert (= 3 (llvm:debug-scope-line (llvm:function-debug-scope function))))
      (assert (string= "source.lisp"
                       (llvm:debug-file-filename (llvm:debug-scope-file scope))))
      (llvm:emit-function-body function '("input")
        (lambda (parameters)
          (assert (null (llvm:builder-debug-location llvm:*builder*)))
          (llvm:set-debug-location llvm:*builder* location)
          (assert (= 4 (llvm:debug-location-line
                        (llvm:builder-debug-location llvm:*builder*))))
          (llvm:ret (first parameters))))
      (llvm:finish-debug-function info function)
      (llvm:finish-debug-info info)
      (assert (llvm:verify-module module)))))

(defun test-funclet-example ()
  (llvm:with-objects ()
    (let* ((module (zk-llvm/examples/funclet-ir:read-and-release-module))
           (function (llvm:find-function module "read_and_release"))
           (unwind (llvm:find-block function "unwind")))
      (destructuring-bind (pad call return) (llvm:block-instructions unwind)
        (assert (eq :cleanuppad (llvm:instruction-opcode pad)))
        (assert (eq :cleanupret (llvm:instruction-opcode return)))
        (assert (string= "funclet"
                         (llvm:operand-bundle-tag (first (llvm:call-operand-bundles call)))))
        (assert (llvm:value-equal-p
                 (llvm:instruction-result pad)
                 (first (llvm:operand-bundle-operands
                         (first (llvm:call-operand-bundles call)))))))
      (assert (llvm:verify-module module)))))
