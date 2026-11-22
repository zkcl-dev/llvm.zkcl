;;;; Public entry point for the Tiny compiler example.
;;;; Start with demo.sexp. frontend.lisp checks each function
;;;; against the program's signatures. interpreter.lisp evaluates that
;;;; checked tree and llvm.lisp lowers the same tree to SSA. This file
;;;; handles the command line and runs both backends against the demo cases.

(in-package #:zk-llvm/examples/compiler)

;; Capture the data path before LOAD can bind LOAD-TRUENAME to a FASL.
(defparameter *demo-source*
  #.(namestring (merge-pathnames "demo.sexp"
                                 (or *compile-file-truename* *load-truename*))))

(defun entry-types (definition arguments)
  (let* ((signature (checked-function-signature definition))
         (name (function-signature-name signature))
         (parameters (mapcar #'cdr (function-signature-parameters signature))))
    ;; The CLI and C examples share this stable int64_t entry-point ABI.
    (unless (and (eq (function-signature-result signature) :i64)
                 (every (lambda (type) (eq type :i64)) parameters))
      (error "Entry ~S must take and return :I64." name))
    (unless (= (length arguments) (length parameters))
      (error "~S expects ~D arguments." name (length parameters)))
    parameters))

(defun print-result (name arguments result)
  (format t "~A(~{~D~^, ~}) = ~D~%" name arguments result))

(defun parse-argument (text)
  (let ((value (parse-integer text)))
    (unless (typep value '(signed-byte 64))
      (error "Argument ~S is outside the signed 64-bit range." text))
    value))

(defun print-usage (&optional (stream *standard-output*))
  (format stream "Run the demo with zkcl run --bin tiny.~%~
                  Add -- followed by one of these command lines.~%~
                  ~%~
                  SOURCE ENTRY [INTEGER ...]                 Run the JIT.~%~
                  --repl [SOURCE]                            Read Tiny forms from standard input.~%~
                  --interpret SOURCE ENTRY [INTEGER ...]     Run the interpreter.~%~
                  --emit-ir SOURCE OUTPUT [TRIPLE]           Write unoptimized LLVM IR.~%~
                  --emit-object SOURCE OUTPUT [TRIPLE]       Write an optimized object file.~%~
                  --emit-objects SOURCE OUTPUT [...]          Compile each source to its host object file.~%~
                  --emit-assembly SOURCE OUTPUT [TRIPLE]      Write optimized assembly.~%~
                  ~%~
                  SOURCE can be examples/tiny/demo.sexp. Try sum_squares -2 3.~%~
                  Tiny types are :I64, :I1, and nested (pair LEFT-TYPE RIGHT-TYPE).~%~
                  Use (pair LEFT RIGHT), (first PAIR), and (second PAIR) in expressions.~%~
                  In the REPL, enter (fn ...) definitions and expressions such as (+ 20 22).~%~
                  Omit TRIPLE to generate code for the host.~%~
                  For C callers, use entries with only :I64 parameters and result (int64_t).~%"))

(defun usage-error ()
  (print-usage *error-output*)
  (error "Invalid compiler arguments."))

(defun run-source (arguments &key interpret)
  (unless (cdr arguments) (usage-error))
  (destructuring-bind (source name &rest inputs) arguments
    (let* ((definitions (check-program (read-program source)))
           (parameters (mapcar #'parse-argument inputs))
           (definition (find-definition name definitions))
           (types (entry-types definition parameters)))
      (print-result name parameters
                    (if interpret
                        (interpret-function definition parameters definitions)
                        (llvm:with-objects ()
                          (llvm:with-jit (jit)
                            (llvm:jit-add-module jit (compile-program definitions :jit jit))
                            (call-jit jit name :i64 types parameters))))))))

(defun emit-output-file (arguments format)
  (unless (<= 2 (length arguments) 3) (usage-error))
  (destructuring-bind (source output &optional triple) arguments
    (let ((definitions (check-program (read-program source))))
      (when (equal (truename source) (probe-file output))
        (error "Source and output refer to the same file ~S." source))
      (llvm:with-objects ()
        (let* ((target (llvm:make-target-machine :triple (or triple (llvm:host-triple))))
               ;; Keep the IR's blocks, PHIs, and aggregate operations visible.
               ;; Native output instead demonstrates the optimized program.
               (module (compile-program definitions :target-machine target
                                        :optimize (unless (eq format :ir) :o2))))
          (llvm:write-module module output :format format
                                          :target-machine (unless (eq format :ir) target)
                                          :if-exists :supersede))))))

(defun emit-native-objects (arguments)
  (unless (and arguments (evenp (length arguments))) (usage-error))
  (loop for pair on arguments by #'cddr
        do (emit-output-file (list (first pair) (second pair)) :object)))

(defun check-diagnostics ()
  (dolist (case '((((fn "invalid" () :i64 (if 1 2 3))) "Expected :I1")
                  (((fn "invalid" () :i64 missing)) "Unbound variable")
                  (((fn "f" ((x :i64)) :i64 x)
                    (fn "invalid" () :i64 (call "f"))) "expects 1 arguments")
                  (((fn "f" () :i64 0) (fn "f" () :i64 1)) "Duplicate function")
                  (((fn "invalid" () :i64 (fold (x 0 1) (x 0) x))) "Invalid fold bindings")
                  (((fn "invalid" () :i64 (fold (i 0 1) (sum 0) :false))) "Expected :I64")
                  (((fn "invalid" () :i64 (first 1))) "Expected a pair")
                  (((fn "invalid" () (pair :i64 :unknown) (pair 1 :true))) "Invalid Tiny type")
                  (((fn "invalid" ((p (pair :i64 :i1))) :i64 (second p))) "Expected :I64")
                  (((fn "invalid" () :i64 (first (if :true (pair 1 2) (pair 1 :true)))))
                   "Expected (")
                  (((fn "invalid" () :i64 9223372036854775808)) "Invalid expression")))
    (destructuring-bind (source message) case
      (assert (handler-case (progn (check-program source) nil)
                (error (condition) (search message (princ-to-string condition))))))))

(defun run-demo ()
  (check-diagnostics)
  (let ((definitions (check-program (read-program *demo-source*))))
    (llvm:with-objects ()
      (llvm:with-jit (jit)
        (llvm:jit-add-module jit (compile-program definitions :jit jit))
        (dolist (case '(("score" (-2) 0)
                        ("score" (0) 1)
                        ("score" (8) 45)
                        ("score" (9223372036854775807) 0)
                        ("difference" (-9223372036854775808 1) 9223372036854775807)
                        ("odd_flag" (-3) 1)
                        ("odd_flag" (8) 0)
                        ("sum_range" (2 5) 9)
                        ("sum_squares" (-2 3) 10)
                        ("sum_squares" (3 -2) 0)
                        ("sum_squares" (9223372036854775806 9223372036854775807) 4)))
          (destructuring-bind (name arguments expected) case
            (let* ((definition (find-definition name definitions))
                   (reference (interpret-function definition arguments definitions))
                   (result (call-jit jit name :i64 (entry-types definition arguments) arguments)))
              (assert (= result reference expected))
              (print-result name arguments result))))))))

(defun main ()
  (destructuring-bind (&optional command &rest operands) (zk:command-line-arguments)
    (cond
      ((null command) (run-demo))
      ((string= command "--help") (if operands (usage-error) (print-usage)))
      ((string= command "--repl") (if (cdr operands) (usage-error) (run-repl (first operands))))
      ((string= command "--interpret") (run-source operands :interpret t))
      ((string= command "--emit-ir") (emit-output-file operands :ir))
      ((string= command "--emit-object") (emit-output-file operands :object))
      ((string= command "--emit-objects") (emit-native-objects operands))
      ((string= command "--emit-assembly") (emit-output-file operands :assembly))
      (t (run-source (cons command operands))))))
