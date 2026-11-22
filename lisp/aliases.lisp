(in-package #:zk-llvm)

(define-operation (%add-global-alias add-global-alias) "add_global_alias" :global-alias
    (module :handle) (name :cstring) (value-type :handle) (target :handle) (linkage :uint32))
(define-operation find-global-alias "find_global_alias" :global-alias
    (module :handle) (name :cstring))
(define-operation %first-global-alias "first_global_alias" :global-alias (module :handle))
(define-operation %next-global-alias "next_global_alias" :global-alias (alias :handle))
(define-operation global-alias-equal-p "global_alias_eq" :bool (left :handle) (right :handle))
(define-operation global-alias-module "global_alias_module" :module (alias :handle))
(define-operation global-alias-name "global_alias_name" :text (alias :handle))
(define-operation set-global-alias-name "set_global_alias_name" :effect
    (alias :handle) (name :cstring))
(define-operation global-alias-value-type "global_alias_value_type" :type (alias :handle))
(define-operation global-alias-address "global_alias_address" :value (alias :handle))
(define-operation global-alias-address-space "global_alias_address_space" :uint64 (alias :handle))
(define-operation global-alias-target "global_alias_target" :value (alias :handle))
(define-operation (%set-global-alias-target set-global-alias-target) "set_global_alias_target" :effect
    (alias :handle) (target :handle))
(define-operation (%global-alias-linkage global-alias-linkage) "global_alias_linkage" :uint64
    (alias :handle))

(defmethod operand-value ((object global-alias))
  (global-alias-address object))

(defun add-global-alias (module name value-type target &key (linkage :external))
  "Define exact symbol NAME for a global, function, alias, or constant address expression."
  (let ((address (operand-value target)))
    (cl:unless address
      (error 'llvm-error :operation 'add-global-alias :message "Expected an LLVM address."))
    (%add-global-alias module name (resolve-type value-type (module-context module)) address
                       (linkage-index 'add-global-alias linkage))))

(defun module-global-aliases (&optional (module (ensure-module)))
  "Return the global aliases in MODULE in LLVM order."
  (cl:loop for alias = (%first-global-alias module) then (%next-global-alias alias)
           while alias collect alias))

(defun set-global-alias-target (alias target)
  "Change ALIAS's immediate target to a constant address of the same pointer type."
  (let ((address (operand-value target)))
    (cl:unless address
      (error 'llvm-error :operation 'set-global-alias-target :message "Expected an LLVM address."))
    (%set-global-alias-target alias address)))

(defun global-alias-linkage (alias)
  (nth (%global-alias-linkage alias) *linkages*))
