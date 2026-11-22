(in-package #:zk-llvm)

(define-operation (%function-attribute function-attribute) "function_attribute" :pointer
    (function :handle) (index :uint32) (name :cstring) (string :bool))
(define-operation (%function-attributes function-attributes) "function_attributes" :pointer
    (function :handle) (index :uint32))
(define-operation (%call-attribute call-attribute) "call_attribute" :pointer
    (call :handle) (index :uint32) (name :cstring) (string :bool))
(define-operation (%call-attributes call-attributes) "call_attributes" :pointer
    (call :handle) (index :uint32))

(define-operation %attribute-count "attribute_count" :uint64 (attributes :pointer))
(define-operation %attribute-at "attribute_at" :pointer (attributes :pointer) (index :uintptr))
(define-operation %attribute-name "attribute_name" :pointer (attribute :pointer))
(define-operation %attribute-kind-id "attribute_kind_id" :uint64 (attribute :pointer))
(define-operation %attribute-payload "attribute_payload" :uint64 (attribute :pointer))
(define-operation %attribute-integer "attribute_integer" :uint64 (attribute :pointer))
(define-operation %attribute-type "attribute_type" :type (attribute :pointer))
(define-operation %attribute-string "attribute_string" :pointer (attribute :pointer))
(define-operation %attribute-range-bit-width "attribute_range_bit_width" :uint64 (attribute :pointer))
(define-operation %attribute-range-bound "attribute_range_bound" :pointer (attribute :pointer) (upper :bool))

(defun copy-attribute-text (bytes)
  (cl:when bytes
    (unwind-protect
        (let ((length (%bytes-len bytes)))
          (cl:if (zerop length)
                 ""
                 (ffi:read-c-string (invoke 'copy-attribute-text #'%bytes-data (list bytes))
                                    length)))
      (%release bytes))))

(defun copy-attribute-range (attribute)
  (list :bit-width (%attribute-range-bit-width attribute)
        :lower (parse-integer (copy-attribute-text (%attribute-range-bound attribute nil)) :radix 10)
        :upper (parse-integer (copy-attribute-text (%attribute-range-bound attribute t)) :radix 10)))

(defun copy-attribute (attribute)
  (cl:when attribute
    (unwind-protect
        (let ((name (copy-attribute-text (%attribute-name attribute)))
              (id (%attribute-kind-id attribute)))
          (multiple-value-bind (kind value)
              (ecase (%attribute-payload attribute)
                (0 (values :flag t))
                (1 (values :integer (%attribute-integer attribute)))
                (2 (values :type (%attribute-type attribute)))
                (3 (values :string (copy-attribute-text (%attribute-string attribute))))
                (4 (values :opaque nil))
                (5 (values :range (copy-attribute-range attribute))))
            (list :name name :kind-id (cl:unless (zerop id) id) :kind kind :value value)))
      (%release attribute))))

(defun copy-attributes (attributes)
  (unwind-protect
      (cl:loop for index below (%attribute-count attributes)
               collect (copy-attribute (%attribute-at attributes index)))
    (%release attributes)))

(defun function-attribute (function location name &key string)
  "Return the named attribute at LOCATION as a plist, or NIL if absent.
STRING selects a string key that also names a built-in. The plist's :KIND is
:FLAG, :INTEGER, :TYPE, :STRING, :RANGE, or :OPAQUE. A range's :VALUE has
:BIT-WIDTH, :LOWER, and :UPPER, with bounds written as signed integers.
:VALUE is T for a flag and NIL for an opaque payload. :KIND-ID is the LLVM
built-in kind ID, when present."
  (copy-attribute (%function-attribute function (attribute-location-index location) name string)))

(defun function-attributes (function location)
  "List attribute plists at LOCATION in LLVM order. LOCATION is :FUNCTION,
:RETURN, or a zero-based parameter index."
  (copy-attributes (%function-attributes function (attribute-location-index location))))

(defun call-attribute (call location name &key string)
  "Return the attribute explicitly attached to CALL as a plist, or NIL if absent.
NAME and STRING have the same meaning as in FUNCTION-ATTRIBUTE."
  (copy-attribute (%call-attribute call (attribute-location-index location) name string)))

(defun call-attributes (call location)
  "List attributes explicitly attached to CALL at LOCATION in LLVM order.
An integer LOCATION selects a zero-based argument index."
  (copy-attributes (%call-attributes call (attribute-location-index location))))
