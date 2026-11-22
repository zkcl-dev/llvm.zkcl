(in-package #:zk-llvm)

(defvar *context* nil
  "Default LLVM context for type specifiers, constants, and modules.")

(defvar *module* nil
  "Default LLVM module for declarations, globals, and function definitions.")

(defvar *function* nil
  "Current LLVM function being constructed.")

(defvar *builder* nil
  "Current LLVM instruction builder.")

(defvar *native-library*
  (cl:if (ffi:find-symbol-pointer :host "zk_llvm_make_context")
         :host
         (let ((path (zk:getenv "ZK_LLVM_LIBRARY")))
           (cl:unless path
             (error "Set ZK_LLVM_LIBRARY to the libzkcl_llvm_api shared library (see ./install-llvm)."))
           (ffi:open-library (namestring (truename path))))))

(defmacro define-native (name suffix result &rest parameters)
  (let ((symbol (concatenate 'string "zk_llvm_" suffix))
        (binding (gensym "BINDING"))
        (arguments (mapcar #'first parameters)))
    `(progn
       (defparameter ,binding
         (ffi:bind-function *native-library* ,symbol ,result
                            ,@(mapcar #'second parameters)))
       (defun ,name ,arguments (funcall ,binding ,@arguments)))))

(define-native %release "release" :void (pointer :pointer))
(define-native %free-error "free_error" :void (message :pointer))
(define-native %text-data "text_data" :pointer (failure :pointer) (text :pointer))
(define-native %bytes-data "bytes_data" :pointer (failure :pointer) (bytes :pointer))
(define-native %bytes-len "bytes_len" :uintptr (bytes :pointer))
(define-native %handle-live "handle_alive_p" :uint64 (failure :pointer) (object :pointer))

(define-condition llvm-error (error)
  ((operation :initarg :operation :reader llvm-error-operation)
   (message :initarg :message :reader llvm-error-message))
  (:report (lambda (condition stream)
             (format stream "~A: ~A" (llvm-error-operation condition)
                     (llvm-error-message condition)))))

(defun operation-index (operation value choices)
  (cl:or (position value choices)
         (error 'llvm-error :operation operation
                :message (format nil "Unknown option ~S. Expected one of ~S." value choices))))

(defun invoke (operation binding arguments)
  (ffi:with-foreign-memory (failure (ffi:type-size :pointer))
    (let* ((result (apply binding failure arguments))
           (message (ffi:mem-ref failure :pointer)))
      (cl:when message
        (unwind-protect (error 'llvm-error :operation operation :message (ffi:read-c-string message))
          (%free-error message)))
      result)))

(defstruct (handle (:constructor nil) (:copier nil))
  (cell nil :read-only t)
  owner)

(defstruct (context (:include handle) (:constructor %make-context-handle (cell)) (:copier nil)))
(defstruct (module (:include handle) (:constructor %make-module-handle (cell)) (:copier nil)))
(defstruct (module-edit (:include handle) (:constructor %make-module-edit (cell)) (:copier nil)))
(defstruct (builder (:include handle) (:constructor %make-builder-handle (cell)) (:copier nil)))
(defstruct (ir-type (:include handle) (:constructor %make-type-handle (cell)) (:predicate type-p) (:copier nil)))
(defstruct (value (:include handle) (:constructor %make-value-handle (cell)) (:copier nil)))
(defstruct (metadata (:include handle) (:constructor %make-metadata-handle (cell)) (:copier nil)))
(defstruct (ir-function (:include handle) (:constructor %make-function-handle (cell)) (:predicate function-p) (:copier nil)))
(defstruct (ir-block (:include handle) (:constructor %make-block-handle (cell)) (:predicate block-p) (:copier nil)))
(defstruct (instruction (:include handle) (:constructor %make-instruction-handle (cell)) (:copier nil)))
(defstruct (operand-bundle (:include handle) (:constructor %make-operand-bundle-handle (cell)) (:copier nil)))
(defstruct (global (:include handle) (:constructor %make-global-handle (cell)) (:copier nil)))
(defstruct (global-alias (:include handle) (:constructor %make-global-alias-handle (cell)) (:copier nil)))
(defstruct (comdat (:include handle) (:constructor %make-comdat-handle (cell)) (:copier nil)))
(defstruct (target-machine (:include handle) (:constructor %make-target-machine-handle (cell)) (:copier nil)))
(defstruct (data-layout (:include handle) (:constructor %make-data-layout-handle (cell)) (:copier nil)))
(defstruct (debug-info (:include handle) (:constructor %make-debug-info-handle (cell)) (:copier nil)))
(defstruct (debug-file (:include handle) (:constructor %make-debug-file-handle (cell)) (:copier nil)))
(defstruct (debug-scope (:include handle) (:constructor %make-debug-scope-handle (cell)) (:copier nil)))
(defstruct (debug-location (:include handle) (:constructor %make-debug-location-handle (cell)) (:copier nil)))
(defstruct (debug-type (:include handle) (:constructor %make-debug-type-handle (cell)) (:copier nil)))
(defstruct (debug-member (:include handle) (:constructor %make-debug-member-handle (cell)) (:copier nil)))
(defstruct (debug-variable (:include handle) (:constructor %make-debug-variable-handle (cell)) (:copier nil)))
(defstruct (jit (:include handle) (:constructor %make-jit-handle (cell)) (:copier nil)))
(defstruct (jit-symbol (:include handle) (:constructor %make-jit-symbol-handle (cell)) (:copier nil)))
(defstruct (resource-tracker (:include handle) (:constructor %make-resource-tracker-handle (cell)) (:copier nil)))

(defvar *object-scope* nil)
(defvar *foreign-arguments* nil)

(defun handle-open-p (object)
  (cl:and (handle-p object) (cl:not (null (car (handle-cell object))))))

(defun handle-alive-p (object)
  "Return true when OBJECT is a live, unreleased LLVM handle."
  (cl:when (handle-open-p object)
    (let ((*foreign-arguments* (cons object *foreign-arguments*)))
      (cl:not (zerop (invoke 'handle-alive-p #'%handle-live (list (car (handle-cell object)))))))))

(defun handle-status-label (object)
  (cl:if (car (handle-cell object)) ":ABANDONED" ":RELEASED"))

(defun release-cell (cell)
  (let ((pointer (car cell)))
    (cl:when pointer
      (setf (car cell) nil)
      (%release pointer))))

(defun release-later (cell)
  (lambda () (release-cell cell)))

(defun wrap-handle (pointer constructor)
  (cl:when pointer
    (let* ((cell (list pointer))
           (object (funcall constructor cell)))
      (zk:finalize object (release-later cell))
      (cl:when *object-scope*
        (tl:push-back! *object-scope* object))
      object)))

(defun ensure-handle-releasable (object)
  (cl:when (member object *foreign-arguments*)
    (error 'llvm-error :operation 'release :message "LLVM handle is in use.")))

(defun release-handle (object)
  (release-cell (handle-cell object))
  (setf (handle-owner object) nil)
  (zk:cancel-finalization object)
  nil)

(defun release (object)
  "Release an LLVM handle on its creating thread. Releasing it twice has no effect."
  (ensure-handle-releasable object)
  (release-handle object))

(defun release-scope (scope)
  (cl:loop for index downfrom (cl:1- (tl:length scope)) to 0
        do (release (tl:ref scope index))))

(defmacro with-objects (() &body body)
  "Release the handles created inside BODY when its dynamic scope exits."
  (let ((scope (gensym "SCOPE")))
    `(let ((,scope (tl:mutable-treelist)))
       (unwind-protect
            (let ((*object-scope* ,scope))
              (locally ,@body))
         (release-scope ,scope)))))

(defmacro with-llvm-objects (() &body body)
  "Alias for WITH-OBJECTS."
  `(with-objects () ,@body))

(defun handle-pointer (object)
  (cl:or (car (handle-cell object))
         (error "LLVM handle has been released.")))

(defun optional-handle-pointer (object)
  (cl:when object (handle-pointer object)))

(defun sequence->list (objects)
  "Convert a proper list, vector, or zkcl treelist into a proper list."
  (cl:cond
    ((null objects) nil)
    ((consp objects)
     (cl:unless (list-length objects)
       (error "Expected a finite proper list of LLVM operands."))
     objects)
    ((cl:or (tl:treelist-p objects) (tl:mutable-treelist-p objects))
     (tl:to-list objects))
    ((vectorp objects)
     (coerce objects 'list))
    (t (error "Expected a proper list, vector, or treelist of LLVM operands, got ~S." objects))))

(defun call-with-handles (objects function &key allow-nil)
  (let* ((items (sequence->list objects))
         (count (length items)))
    (cl:if (zerop count)
           (funcall function nil 0)
           (let ((width (ffi:type-size :pointer))
                 (*foreign-arguments* (append items *foreign-arguments*)))
             (ffi:with-foreign-memory (buffer (cl:* count width))
               (cl:loop for object in items
                     for offset from 0 by width
                     do (ffi:mem-set buffer :pointer offset
                                     (cl:if allow-nil
                                            (optional-handle-pointer object)
                                            (handle-pointer object))))
               (funcall function buffer count))))))

(defun call-with-bytes (octets function)
  (check-type octets (vector (unsigned-byte 8)))
  (let ((count (length octets)))
    (cl:if (zerop count)
           (funcall function nil 0)
           (ffi:with-foreign-memory (buffer count)
             (ffi:write-octets buffer octets :end count)
             (funcall function buffer count)))))

(defun call-with-octets (text-or-octets function)
  "Pass the UTF-8 bytes of a string or an octet vector to FUNCTION. Embedded NULs are preserved."
  (etypecase text-or-octets
    (string
     (let ((count (ffi:utf8-prefix text-or-octets 0 nil nil)))
       (ffi:with-foreign-memory (buffer (max 1 count))
         (ffi:write-utf8 buffer text-or-octets 0 nil)
         (funcall function buffer count))))
    ((vector (unsigned-byte 8)) (call-with-bytes text-or-octets function))))

(defun octets-or-text (octets)
  "Decode valid UTF-8, preserving undecodable tag and assembly bytes as an octet vector."
  (cl:if (zerop (length octets))
         ""
         (call-with-bytes octets
           (lambda (data length)
             (handler-case (ffi:read-c-string data length)
               (error () octets))))))

(defun copy-text (operation text)
  (unwind-protect (ffi:read-c-string (invoke operation #'%text-data (list text)))
    (%release text)))

(defun copy-bytes (operation bytes)
  (unwind-protect
      (let* ((data (invoke operation #'%bytes-data (list bytes)))
             (count (%bytes-len bytes))
             (octets (make-array count :element-type '(unsigned-byte 8))))
        (ffi:read-octets data octets :end count)
        octets)
    (%release bytes)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun handle-constructor-for (result)
    (case result
      (:context '%make-context-handle)
      (:module '%make-module-handle)
      (:module-edit '%make-module-edit)
      (:builder '%make-builder-handle)
      (:type '%make-type-handle)
      (:value '%make-value-handle)
      (:metadata '%make-metadata-handle)
      (:function '%make-function-handle)
      (:block '%make-block-handle)
      (:instruction '%make-instruction-handle)
      (:operand-bundle '%make-operand-bundle-handle)
      (:global '%make-global-handle)
      (:global-alias '%make-global-alias-handle)
      (:comdat '%make-comdat-handle)
      (:target-machine '%make-target-machine-handle)
      (:data-layout '%make-data-layout-handle)
      (:debug-info '%make-debug-info-handle)
      (:debug-file '%make-debug-file-handle)
      (:debug-scope '%make-debug-scope-handle)
      (:debug-location '%make-debug-location-handle)
      (:debug-type '%make-debug-type-handle)
      (:debug-member '%make-debug-member-handle)
      (:debug-variable '%make-debug-variable-handle)
      (:jit '%make-jit-handle)
      (:jit-symbol '%make-jit-symbol-handle)
      (:resource-tracker '%make-resource-tracker-handle))))

(defmacro define-operation (names suffix result &rest parameters)
  (let ((name (cl:if (consp names) (first names) names))
        (operation (cl:if (consp names) (second names) names))
        (binding (gensym "NATIVE"))
        (failure (gensym "FAILURE"))
        (native-result (case result
                         ((:bool :uint64) :uint64)
                         (otherwise :pointer)))
        (native-parameters
          (cl:loop for (arg type) in parameters
                collect (list arg (case type
                                    ((:handle :optional-handle) :pointer)
                                    (:bool :uint8)
                                    (otherwise type))))))
    (let* ((arguments (cl:loop for (arg type) in parameters
                            collect (case type
                                      (:handle `(handle-pointer ,arg))
                                      (:optional-handle `(optional-handle-pointer ,arg))
                                      (:bool `(cl:if ,arg 1 0))
                                      (otherwise arg))))
           (handles (cl:loop for (arg type) in parameters
                          when (member type '(:handle :optional-handle)) collect arg))
           (call `(invoke ',operation #',binding (list ,@arguments)))
           (constructor (handle-constructor-for result))
           (result (cl:if constructor
                          `(wrap-handle ,call #',constructor)
                          (ecase result
                            (:text `(copy-text ',operation ,call))
                            (:optional-text `(let ((text ,call))
                                               (cl:when text (copy-text ',operation text))))
                            (:bytes `(copy-bytes ',operation ,call))
                            (:bool `(cl:not (zerop ,call)))
                            ((:pointer :uint64) call)
                            (:effect `(progn ,call nil))
                            (:verified `(progn ,call t))))))
      `(progn
         (define-native ,binding ,suffix ,native-result (,failure :pointer) ,@native-parameters)
         (defun ,name ,(mapcar #'first parameters)
           ,(cl:if handles
                   `(let ((*foreign-arguments* (list* ,@handles *foreign-arguments*))) ,result)
                   result))))))
