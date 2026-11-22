;;;; Run from llvm.zkcl with an LLVM-enabled zkcl on PATH.
;;;;   rtk proxy zkcl run tests/compiler-native.lisp
;;;; Pass -- /path/to/zkcl /path/to/cc to select the child executables.
;;;; CC can also name a single compiler executable.
(require "asdf")
(asdf:load-system "zk-llvm/tiny")

(defpackage #:zk-llvm/tests/compiler-native
  (:use #:cl)
  (:local-nicknames (#:llvm #:zk-llvm)
                    (#:tiny #:zk-llvm/examples/compiler)))

(in-package #:zk-llvm/tests/compiler-native)

(defparameter *project*
  (uiop:pathname-parent-directory-pathname
   (uiop:pathname-directory-pathname *load-truename*)))
(defparameter *demo-source* (merge-pathnames "examples/tiny/demo.sexp" *project*))
(defparameter *test-source* (merge-pathnames "tests/fixtures/tiny/native.sexp" *project*))
(defparameter *corpus-source* (merge-pathnames "tests/fixtures/tiny/corpus.sexp" *project*))
(defparameter *scratch-root*
  (merge-pathnames ".agents/tmp/llvm.zkcl/e2e/"
                   (uiop:pathname-parent-directory-pathname *project*)))
(defconstant +i64-min+ (- (ash 1 63)))
(defconstant +i64-max+ (1- (ash 1 63)))

(defstruct test-case source name arguments expected (reference-p t))

(defun check-repl-replacement ()
  ;; Existing callers must reach the replacement after a successful submission.
  (let ((session (tiny::make-repl-session :jit (llvm:make-jit))))
    (unwind-protect
         (flet ((submit (source)
                  (with-output-to-string (output)
                    (tiny::submit-repl-forms
                     session
                     (tiny::call-with-tiny-reader
                      (lambda ()
                        (with-input-from-string (input source)
                          (tiny::read-forms input))))
                     output))))
           (submit "(fn \"step\" ((x :i64)) :i64 (+ x 1))
                    (fn \"caller\" ((x :i64)) :i64 (call \"step\" x))")
           (assert (string= (format nil "11~%") (submit "(call \"caller\" 10)")))
           (assert (handler-case
                       (progn (submit "(fn \"step\" ((x :i64)) :i1 (< x 0))") nil)
                     (error (condition) (search "Expected :I64" (princ-to-string condition)))))
           (assert (string= (format nil "11~%") (submit "(call \"caller\" 10)")))
           (submit "(fn \"step\" ((x :i64)) :i64 (+ x 2))")
           (assert (string= (format nil "12~%") (submit "(call \"caller\" 10)")))
           ;; The wrapper's scalar buffer defines the host ABI for nested pairs
           ;; and i1 values.
           (assert (string= (format nil "((12 :TRUE) :FALSE)~%")
                            (submit "(pair (pair (call \"caller\" 10) :true) :false)"))))
      (tiny::dispose-repl-jit (tiny::repl-session-jit session)))))

(defun signed-i64 (integer)
  (+ +i64-min+ (mod (- integer +i64-min+) (ash 1 64))))

(defun make-cases ()
  (let ((cases nil))
    (flet ((add (source name expected &rest arguments)
             (push (make-test-case :source source :name name
                                   :arguments arguments :expected expected)
                   cases)))
      (dolist (n (list +i64-max+ +i64-min+ -10 -3 -2 -1 0 1 2 3 7 12 31 64 127))
        (let ((terms (max 0 (signed-i64 (1+ n)))))
          (add *demo-source* "score" (floor (* terms (1+ terms)) 2) n)))
      (dolist (n '(-63 -16 -7 -3 -2 -1 0 1 2 3 7 16 63))
        (add *demo-source* "odd_flag" (mod (abs n) 2) n))
      (loop for (a b) in `((,+i64-min+ 1) (,+i64-min+ -1) (,+i64-min+ 0)
                            (,+i64-min+ ,+i64-min+) (,+i64-min+ ,+i64-max+)
                            (,+i64-max+ ,+i64-min+) (,+i64-max+ -1) (,+i64-max+ 0)
                            (,+i64-max+ 1) (,+i64-max+ 2) (,+i64-max+ ,+i64-max+)
                            (0 0) (-13 -7) (-9 13) (-1 2) (2 -1) (9 3))
            for total = (signed-i64 (+ a b))
            do (add *demo-source* "difference" (signed-i64 (- a b)) a b)
               (add *test-source* "wrapped_sum" total a b)
               (add *test-source* "wrapped_product" (signed-i64 (* a b)) a b)
               (add *test-source* "signed_order" (signum (- a b)) a b)
               (add *test-source* "nested_scope" (signed-i64 (* 3 (max total 0))) a b))
      (loop for (start end) in `((,+i64-min+ ,(+ +i64-min+ 2))
                                (,(- +i64-max+ 2) ,+i64-max+) (-4 4) (-6 1) (-3 -1)
                                (-4 -4) (-1 -4) (-1 0) (0 0) (0 1) (0 7) (1 -1)
                                (5 2) (,+i64-max+ ,+i64-min+) (,+i64-max+ ,+i64-max+))
            for squares = (loop for index from start below end sum (* index index))
            ;; The three inner iterations add -i + i + 2*i after shadowing i.
            for contribution = (* 2 (loop for index from start below end sum (1+ index)))
            for negative-count = (max 0 (- (min end 0) start))
            do (add *demo-source* "sum_squares" (signed-i64 squares) start end)
               (dolist (seed (list +i64-min+ +i64-max+))
                 (add *test-source* "nested_fold" (signed-i64 (+ seed contribution))
                      start end seed)
                 (add *test-source* "fold_parity" (mod (+ negative-count (if (minusp seed) 1 0)) 2)
                      start end seed)))
      (loop for (a b c) in `((,+i64-max+ -1 ,+i64-min+) (,+i64-min+ 2 3)
                             (0 0 0) (9 2 -5) (-8 7 13) (1 -1 ,+i64-max+))
            do (add *test-source* "argument_order" (signed-i64 (+ (* 5 a) (* -6 b) (* 3 c)))
                    a b c))
      (add *test-source* "least_i64" +i64-min+))
    (nreverse cases)))

(defun generated-module (name parameters body inputs expected)
  (list (list `(fn ,name ,(loop for parameter in parameters collect (list parameter :i64))
                :i64 ,body))
        (loop for arguments in inputs
              collect (list name arguments (signed-i64 (apply expected arguments))))))

(defun arithmetic-modules ()
  ;; Both operand positions receive nested computations with all ordered
  ;; triples of arithmetic operators. Integer-ring equivalence lets the
  ;; reference calculation wrap only its final result.
  (let ((operators '(+ - *))
        (inputs `((0 0 0 0) (2 -3 5 -7) (-11 23 -3 7)
                  (,+i64-max+ ,+i64-min+ 1 -1) (,+i64-min+ 0 ,+i64-max+ 2))))
    (loop for first in operators for i from 1 append
      (loop for second in operators for j from 1 append
        (loop for third in operators for k from 1 append
          (list
           (generated-module (format nil "tree_left_~D~D~D" i j k) '(a b c d)
                             `(,third (,second (,first a b) c) d) inputs
                             (lambda (a b c d)
                               (funcall third (funcall second (funcall first a b) c) d)))
           (generated-module (format nil "tree_right_~D~D~D" i j k) '(a b c d)
                             `(,first a (,second b (,third c d))) inputs
                             (lambda (a b c d)
                               (funcall first a (funcall second b (funcall third c d)))))))))))

(defun comparison-modules ()
  ;; Some joins carry i64 values while the predicate join carries an i1.
  ;; Wrap before signed comparison, which is sensitive to intermediate bits.
  (let ((operators '(+ - *))
        (inputs `((-3 7 2) (7 -3 2) (3 3 1) (0 1 -1)
                  (,+i64-max+ ,+i64-min+ 2) (,+i64-min+ ,+i64-max+ -1))))
    (loop for on-less in operators for i from 1 append
      (loop for otherwise in operators for j from 1 append
        (loop for shape in '(:value :predicate :nested) for k from 1
              for lower = `(,on-less a c)
              for upper = `(,otherwise b c)
              for body = (ecase shape
                           (:value `(let ((p :i1 (< a b))) (if p ,lower ,upper)))
                           (:predicate `(if (if (< a b) (< ,lower c) (< ,upper c)) a b))
                           (:nested `(if (< a b)
                                         (if (< ,lower b) ,lower b)
                                         (if (< a ,upper) ,upper a))))
              collect (generated-module
                       (format nil "compare_join_~D~D~D" i j k) '(a b c) body inputs
                       (lambda (a b c)
                         (let ((lower (signed-i64 (funcall on-less a c)))
                               (upper (signed-i64 (funcall otherwise b c))))
                           (ecase shape
                             (:value (if (< a b) lower upper))
                             (:predicate (if (< (if (< a b) lower upper) c) a b))
                             (:nested (if (< a b) (min lower b) (max a upper))))))))))))

(defun fold-modules ()
  ;; Emit a direct back edge, a back edge after an i64 branch join, and one
  ;; after an inner loop. The reference unrolls the two-step inner update.
  (let ((inputs `((-3 3 5 -2) (3 -2 -4 7) (0 0 ,+i64-min+ 0) (0 5 ,+i64-max+ 2)
                  (,+i64-min+ ,(+ +i64-min+ 2) -1 2) (,(- +i64-max+ 2) ,+i64-max+ 3 0))))
    (loop for update in '(+ - *) for i from 1 append
      (loop for term in '(index (+ index factor) (* index factor)) for j from 1 append
        (loop for shape in '(:plain :branch :nested) for k from 1
              for body = (ecase shape
                           (:plain `(,update acc ,term))
                           (:branch `(if (< index 0) (,update acc ,term) (+ acc 1)))
                           (:nested `(fold (inner 0 2) (acc acc)
                                       (,update acc (+ ,term inner)))))
              collect (generated-module
                       (format nil "fold_update_~D~D~D" i j k) '(start end seed factor)
                       `(fold (index start end) (acc seed) ,body) inputs
                       (lambda (start end seed factor)
                         (loop with result = seed
                               for position from start below end
                               for value = (ecase j (1 position) (2 (+ position factor))
                                                   (3 (* position factor)))
                               do (setf result
                                        (ecase shape
                                          (:plain (funcall update result value))
                                          (:branch (if (minusp position)
                                                       (funcall update result value)
                                                       (1+ result)))
                                          (:nested (funcall update (funcall update result value)
                                                            (1+ value)))))
                               finally (return result)))))))))

(defun write-corpus-module (source module reference-p)
  (destructuring-bind (definitions cases) module
    (with-open-file (stream source :direction :output :if-exists :error)
      (dolist (definition definitions)
        (write definition :stream stream)
        (terpri stream)))
    (loop for (name arguments expected) in cases
          collect (make-test-case :source source :name name :arguments arguments
                                  :expected expected :reference-p reference-p))))

(defun make-corpus-cases (directory)
  (with-standard-io-syntax
    (let ((*package* (find-package '#:zk-llvm/tests/compiler-native))
          (*read-eval* nil)
          (eof (gensym)))
      (with-open-file (fixture *corpus-source*)
        (nconc
         (loop for index from 1
               for module = (read fixture nil eof)
               for source = (merge-pathnames (format nil "corpus-~D.tiny.sexp" index) directory)
               until (eq module eof)
               append (write-corpus-module source module t))
         (loop for index from 1
               for module in (nconc (arithmetic-modules) (comparison-modules) (fold-modules))
               for source = (merge-pathnames (format nil "generated-~D.tiny.sexp" index) directory)
               append (write-corpus-module source module nil)))))))

(defun c-i64 (integer)
  (cond ((= integer +i64-min+) "INT64_MIN")
        ((minusp integer) (format nil "-INT64_C(~D)" (- integer)))
        (t (format nil "INT64_C(~D)" integer))))

(defun write-c-driver (pathname cases entries)
  (with-open-file (stream pathname :direction :output :if-exists :error)
    (format stream "#include <inttypes.h>~%#include <stdio.h>~%~%")
    (dolist (entry entries)
      (format stream "extern int64_t ~A(~{~A~^, ~});~%" (test-case-name entry)
              (or (mapcar (constantly "int64_t") (test-case-arguments entry)) '("void"))))
    (format stream "~%int main(void) {~%")
    (dolist (case cases)
      (format stream "  printf(\"%\" PRId64 \"\\n\", ~A(~{~A~^, ~}));~%"
              (test-case-name case) (mapcar #'c-i64 (test-case-arguments case))))
    (format stream "  return 0;~%}~%")))

;; Bundled UIOP supplies shell quoting. Its RUN-PROGRAM has no zkcl backend.
(defun run-command (arguments &optional output)
  (let* ((system (zk-ffi:bind-function :default "system" :int32 :cstring))
         (command (format nil "exec ~A </dev/null~@[ >~A~]"
                          (uiop:escape-sh-command arguments)
                          (when output (uiop:escape-sh-token (namestring output)))))
         (status (funcall system command)))
    (unless (zerop status)
      (error "Command ~S failed with wait status ~D." arguments status))))

(defun make-scratch-directory ()
  (let* ((template (namestring (merge-pathnames "compiler-XXXXXX" *scratch-root*)))
         (capacity (1+ (* 4 (length template))))
         (mkdtemp (zk-ffi:bind-function :default "mkdtemp" :pointer :pointer)))
    (ensure-directories-exist template)
    (zk-ffi:with-foreign-memory (buffer capacity)
      (zk-ffi:write-c-string buffer template capacity)
      (unless (funcall mkdtemp buffer)
        (error "Cannot create a scratch directory under ~A." *scratch-root*))
      (uiop:ensure-directory-pathname (zk-ffi:read-c-string buffer)))))

(defun result-line (case)
  (format nil "~A(~{~D~^, ~}) = ~D" (test-case-name case)
          (test-case-arguments case) (test-case-expected case)))

(defun check-native-output (pathname cases)
  (with-open-file (stream pathname)
    (dolist (case cases)
      (let ((line (read-line stream nil nil)))
        (unless (equal (format nil "~D" (test-case-expected case)) line)
          (error "Native result for ~A: expected ~A, got ~S."
                 (file-namestring (test-case-source case)) (result-line case) line))))
    (when (read-line stream nil nil)
      (error "Native driver printed extra results after ~D cases." (length cases)))))

(defun check-lisp-output (compiler pathname case interpreter-p)
  (run-command (append compiler (when interpreter-p '("--interpret"))
                       (list (namestring (test-case-source case)) (test-case-name case))
                       (mapcar (lambda (argument) (format nil "~D" argument))
                               (test-case-arguments case)))
               pathname)
  (with-open-file (stream pathname)
    (let ((last-line (loop with last-line = nil
                           for line = (read-line stream nil nil)
                           while line do (setf last-line line)
                           finally (return last-line))))
      (unless (equal (result-line case) last-line)
        (error "~A result for ~A: expected ~A, got ~S."
               (if interpreter-p "Interpreter" "JIT")
               (file-namestring (test-case-source case)) (result-line case) last-line)))))

(defun run-workload (zkcl cc directory)
  (check-repl-replacement)
  (let* ((cases (nconc (make-cases) (make-corpus-cases directory)))
         (entries (remove-duplicates cases :key #'test-case-name :test #'string= :from-end t))
         (reference-entries (remove-if-not #'test-case-reference-p entries))
         (sources (remove-duplicates (mapcar #'test-case-source cases) :test #'equal :from-end t))
         (objects (loop for index from 1 to (length sources)
                        collect (merge-pathnames (format nil "module-~D.o" index) directory)))
         (driver (merge-pathnames "driver.c" directory))
         (executable (merge-pathnames "compiler-native" directory))
         (output (merge-pathnames "output.txt" directory))
         (compiler (list "env" (format nil "CL_SOURCE_REGISTRY=~A:" (namestring *project*))
                         (format nil "ASDF_OUTPUT_TRANSLATIONS=~A:~Afasl/"
                                 (namestring *project*) (namestring directory))
                         zkcl "run" (namestring (merge-pathnames "tests/tiny-program.lisp" *project*))
                         "--")))
    (let ((corpus-arguments
            (loop for source in sources for object in objects
                  for paths = (list (namestring source) (namestring object))
                  if (member source (list *demo-source* *test-source*) :test #'equal)
                    do (run-command (append compiler (cons "--emit-object" paths)))
                  else append paths)))
      (run-command (append compiler (cons "--emit-objects" corpus-arguments))))
    (write-c-driver driver cases entries)
    (run-command (append (list cc "-std=c99" "-Wall" "-Wextra" "-Werror" (namestring driver))
                         (mapcar #'namestring objects) (list "-o" (namestring executable))))
    (run-command (list (namestring executable)) output)
    (check-native-output output cases)
    (dolist (entry reference-entries)
      (check-lisp-output compiler output entry t)
      (check-lisp-output compiler output entry nil))
    (format t "Passed ~D native cases across ~D source modules and ~D cases each for the interpreter and JIT.~%"
            (length cases) (length sources) (length reference-entries))))

(destructuring-bind (&optional (zkcl "zkcl") (cc (or (zk:getenv "CC") "cc")))
    (zk:command-line-arguments)
  (let ((directory (make-scratch-directory)))
    (unwind-protect (run-workload zkcl cc directory)
      (run-command (list "rm" "-rf" (namestring directory))))))
