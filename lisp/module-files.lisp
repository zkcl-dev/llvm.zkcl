(in-package #:zk-llvm)

(define-operation (%read-module read-module) "read_module" :module
    (context :handle) (path :cstring))

(defun read-module (path &key (context *context*))
  "Read and verify LLVM IR or bitcode from PATH in CONTEXT, or in a new context."
  (let* ((owner (cl:or context (make-context)))
         (module (%read-module owner (namestring (truename path)))))
    (setf (handle-owner module) owner)
    module))

(defun module-output-format (path)
  (let ((extension (pathname-type path)))
    (cl:cond
      ((equalp extension "ll") :ir)
      ((equalp extension "bc") :bitcode)
      ((member extension '("s" "asm") :test #'equalp) :assembly)
      ((member extension '("o" "obj") :test #'equalp) :object)
      (t (error 'llvm-error :operation 'write-module
                :message (format nil "Unknown extension on ~A. Pass :FORMAT explicitly." path))))))

(defun write-module (module path &key (format (module-output-format path)) target-machine
                                     (if-exists :error))
  "Write MODULE to PATH as :IR, :BITCODE, :ASSEMBLY or :OBJECT. Infer the format
from the filename by default. Pass :IF-EXISTS :SUPERSEDE to replace an existing file.
TARGET-MACHINE selects the target for assembly and object output. Return the pathname when written."
  (cl:when (cl:and target-machine (member format '(:ir :bitcode)))
    (error 'llvm-error :operation 'write-module
           :message "TARGET-MACHINE applies to assembly and object output."))
  (let* ((content (ecase format
                    (:ir (verify-module module) (module-ir module))
                    (:bitcode (module-bitcode module))
                    (:assembly (emit-assembly module target-machine))
                    (:object (emit-object module target-machine))))
         (element-type (cl:if (stringp content) 'character '(unsigned-byte 8))))
    (with-open-file (stream path :direction :output :element-type element-type
                                :if-exists if-exists :if-does-not-exist :create)
      (cl:when stream
        (write-sequence content stream)
        (pathname stream)))))
