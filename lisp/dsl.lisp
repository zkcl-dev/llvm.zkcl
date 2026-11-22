(in-package #:zk-llvm)

(defstruct ir-loop-control builder break continue recur)

(defvar *loop-control* nil)
(defvar *ir-body-exit* nil)

(defun ensure-builder (&optional (builder *builder*))
  (cl:or builder (error "No active LLVM builder. Use DEFIR, DEFINE-FUNCTION, or WITH-BUILDER.")))

(defun ensure-function (&optional (function *function*))
  (cl:or function (error "No active LLVM function. Use DEFIR or DEFINE-FUNCTION.")))

(defmacro with-context ((&optional (var '*context*) (context-form '(make-context))) &body body)
  "Bind VAR and *CONTEXT* to an LLVM context across BODY."
  `(let* ((,var ,context-form)
          (*context* ,var))
     ,@body))

(defmacro with-module ((var name &key context target-machine jit triple layout verify) &body body)
  "Create and bind a module. Apply TRIPLE and LAYOUT before target compatibility checks."
  (let ((ctx (gensym "CTX"))
        (tm (gensym "TM"))
        (j (gensym "JIT"))
        (target-triple (gensym "TRIPLE"))
        (target-layout (gensym "LAYOUT")))
    `(let* ((,ctx (cl:or ,context *context* (make-context)))
            (*context* ,ctx)
            (,var (make-module ,ctx ,name))
            (*module* ,var)
            (*function* nil)
            (*builder* nil)
            (*loop-control* nil)
            (*ir-body-exit* nil)
            (,tm ,target-machine)
            (,j ,jit)
            (,target-triple ,triple)
            (,target-layout ,layout))
       (cl:when ,target-triple (set-module-target-triple ,var ,target-triple))
       (cl:when ,target-layout (set-module-layout ,var ,target-layout))
       (cl:when ,tm (configure-module-target ,var ,tm))
       (cl:when ,j (jit-configure-module ,j ,var))
       (multiple-value-prog1 (locally ,@body)
         (cl:when ,verify (verify-module ,var))))))

(defmacro define-module ((var name &key context target-machine jit triple layout (verify t) optimize) &body body)
  "Build and return an LLVM module named NAME. Verifies by default and optionally optimizes."
  (let ((opt (gensym "OPT"))
        (verify-p (gensym "VERIFY"))
        (tm (gensym "TM")))
    `(let (,tm)
       (with-module (,var ,name :context ,context :target-machine (setf ,tm ,target-machine)
                         :jit ,jit :triple ,triple :layout ,layout)
         (locally ,@body)
         (let ((,verify-p ,verify)
               (,opt ,optimize))
           (cl:if ,opt
                  (optimize-module ,var (cl:if (eq ,opt t) :o2 ,opt) :target-machine ,tm)
                  (progn
                    (cl:when ,verify-p (verify-module ,var))
                    ,var)))))))

(defmacro with-builder ((&rest options) &body body)
  "Create a temporary builder. Position it with :BLOCK or :BEFORE, then run BODY.
An insertion point also supplies the current LLVM context, module, and function."
  (let ((var (cl:when (cl:and options (cl:not (keywordp (first options))))
               (pop options)))
        (ctx (gensym "CTX"))
        (builder (gensym "BUILDER"))
        (blk (gensym "BLK"))
        (anchor (gensym "ANCHOR"))
        (insertion-block (gensym "INSERTION-BLOCK"))
        (function (gensym "FUNCTION"))
        (module (gensym "MODULE")))
    (destructuring-bind (&key context block before) options
      `(let* ((,blk ,block)
              (,anchor ,before)
              (,insertion-block (cl:or ,blk (cl:when ,anchor (instruction-block ,anchor))))
              (,function (cl:when ,insertion-block (block-function ,insertion-block)))
              (,module (cl:when ,function (function-module ,function)))
              (,ctx (ensure-context (cl:or ,context
                                           (cl:when ,module (module-context ,module))
                                           *context*)))
              (,builder (make-builder ,ctx))
              ,@(cl:when (cl:and var (cl:not (eq var '*builder*))) `((,var ,builder)))
              (*context* ,ctx)
              (*module* (cl:or ,module *module*))
              (*function* (cl:or ,function *function*))
              (*builder* ,builder))
         (unwind-protect
              (progn
                (cl:when (cl:and ,blk ,anchor)
                  (error "WITH-BUILDER accepts either :BLOCK or :BEFORE."))
                (cl:cond (,anchor (position-before ,builder ,anchor))
                         (,blk (position-at-end ,builder ,blk)))
                (locally ,@body))
           (release ,builder))))))

(defmacro with-edit-builder ((edit &key (before nil before-p)) &body body)
  "Emit IR in EDIT immediately before the draft counterpart of BEFORE.
BEFORE is an instruction from the source module. Use DRAFT-VALUE to bring
source operands into scope. Release scoped handles on exit and return zero values."
  (cl:unless before-p (error "WITH-EDIT-BUILDER requires :BEFORE."))
  `(with-objects ()
     (with-builder (:before (draft-instruction ,edit ,before))
       ,@body
       (values))))

(defun c-symbol-name (designator)
  (etypecase designator
    (string designator)
    (symbol (substitute #\_ #\- (string-downcase (symbol-name designator))))))

(defun ensure-value (value &optional expected-type)
  "Coerce VALUE to an LLVM VALUE handle, using EXPECTED-TYPE if VALUE is a Lisp literal."
  (cl:or (operand-value value)
         (cl:if expected-type
                (const expected-type value)
                (error "Cannot infer the LLVM type of ~S. Use (LLVM:CONST TYPE VALUE) to give it a type."
                       value))))

(defun coerce-operand (value type operation)
  (let ((operand (operand-value value)))
    (cl:if operand
           (let ((actual (value-type operand)))
             (cl:unless (type-equal-p type actual)
               (error 'llvm-error :operation operation
                      :message (format nil "Expected an operand of type ~A, got ~A."
                                       (type-ir type) (type-ir actual))))
             operand)
           (const type value))))

(defun infer-literal-type (operands)
  (cl:loop for operand in operands
        for value = (operand-value operand)
        when value return (value-type value)))

(defun coerce-binary-operands (left right)
  (cl:if (cl:and (value-p left) (value-p right))
         (values left right)
         (let ((type (infer-literal-type (list left right))))
           (values (ensure-value left type) (ensure-value right type)))))

(defun floating-value-p (value)
  (let* ((type (value-type value))
         (kind (type-kind type)))
    (member (cl:if (member kind '(:vector :scalable-vector))
                   (type-kind (type-subtype type 0))
                   kind)
            '(:half :bfloat :float :double :x86-fp80 :fp128 :ppc-fp128))))

(defun reduce-ir-binop (operation first rest &optional floating-p)
  (let* ((builder (ensure-builder))
         (type (infer-literal-type (cons first rest)))
         (initial (ensure-value first type))
         (emit (cl:if (cl:and floating-p (floating-value-p initial))
                      #'build-float-binop #'build-int-binop)))
    (reduce (lambda (acc next)
              (funcall emit builder operation acc (ensure-value next type)))
            rest :initial-value initial)))

(defun + (first &rest rest)
  "Add integer or floating-point operands, coercing literals to the operand type."
  (reduce-ir-binop :add first rest t))

(defun - (first &rest rest)
  "Negate FIRST or subtract REST from FIRST (integer or floating-point)."
  (cl:if (null rest)
         (let* ((val (ensure-value first))
                (builder (ensure-builder)))
           (cl:if (floating-value-p val)
                  (build-fneg builder val)
                  (build-int-binop builder :sub (const-zero (value-type val)) val)))
         (reduce-ir-binop :sub first rest t)))

(defun * (first &rest rest)
  "Multiply integer or floating-point operands."
  (reduce-ir-binop :mul first rest t))

(defun int-binop (operation left right &rest options)
  "Emit an integer operation on LLVM operands or literals. See BUILD-INT-BINOP for flags."
  (multiple-value-bind (lhs rhs) (coerce-binary-operands left right)
    (apply #'build-int-binop (ensure-builder) operation lhs rhs options)))

(defun sdiv (left right &rest options)
  (apply #'int-binop :sdiv left right options))

(defun udiv (left right &rest options)
  (apply #'int-binop :udiv left right options))

(defun float-binop (operation left right &rest options)
  "Emit a floating-point operation on LLVM operands or literals. See BUILD-FLOAT-BINOP."
  (multiple-value-bind (lhs rhs) (coerce-binary-operands left right)
    (apply #'build-float-binop (ensure-builder) operation lhs rhs options)))

(defun fneg (value &rest options)
  (apply #'build-fneg (ensure-builder) (ensure-value value) options))

(defun fdiv (left right &rest options)
  (apply #'float-binop :div left right options))

(defun check-integer-fast-math (operation fast-math)
  (cl:when fast-math
    (error 'llvm-error :operation operation :message ":FAST-MATH requires floating-point operands.")))

(defun / (left right &key (signed t) exact (name "") fast-math)
  "Divide LEFT by RIGHT. Integer operands use signed division by default.
Use :SIGNED NIL for unsigned division."
  (multiple-value-bind (lhs rhs) (coerce-binary-operands left right)
    (cl:if (floating-value-p lhs)
           (progn
             (cl:when exact
               (error 'llvm-error :operation '/ :message ":EXACT requires integer division."))
             (build-float-binop (ensure-builder) :div lhs rhs :name name :fast-math fast-math))
           (progn
             (check-integer-fast-math '/ fast-math)
             (build-int-binop (ensure-builder) (cl:if signed :sdiv :udiv) lhs rhs
                              :name name :exact exact)))))

(defun srem (left right &optional (name ""))
  (multiple-value-bind (lhs rhs) (coerce-binary-operands left right)
    (build-int-binop (ensure-builder) :srem lhs rhs name)))

(defun urem (left right &optional (name ""))
  (multiple-value-bind (lhs rhs) (coerce-binary-operands left right)
    (build-int-binop (ensure-builder) :urem lhs rhs name)))

(defun frem (left right &rest options)
  (apply #'float-binop :rem left right options))

(defun rem (left right &key (signed t) (name "") fast-math)
  "Return the remainder of LEFT divided by RIGHT. Integer operands use signed
remainder by default. Use :SIGNED NIL for unsigned remainder."
  (multiple-value-bind (lhs rhs) (coerce-binary-operands left right)
    (cl:if (floating-value-p lhs)
           (build-float-binop (ensure-builder) :rem lhs rhs :name name :fast-math fast-math)
           (progn
             (check-integer-fast-math 'rem fast-math)
             (build-int-binop (ensure-builder) (cl:if signed :srem :urem) lhs rhs name)))))

(defun and (first &rest rest)
  "Emit bitwise AND. Lisp evaluates all operand forms before this function runs."
  (reduce-ir-binop :and first rest))

(defun or (first &rest rest)
  "Emit bitwise OR. Lisp evaluates all operand forms before this function runs."
  (reduce-ir-binop :or first rest))

(defun xor (first &rest rest)
  (reduce-ir-binop :xor first rest))

(defun not (value &optional (name ""))
  (build-not (ensure-builder) (ensure-value value) name))

(defun shl (left right &rest options)
  (apply #'int-binop :shl left right options))

(defun lshr (left right &rest options)
  (apply #'int-binop :lshr left right options))

(defun ashr (left right &rest options)
  (apply #'int-binop :ashr left right options))

(defun compare-ir-operands (operation left right float-predicate int-predicate name fast-math)
  (multiple-value-bind (lhs rhs) (coerce-binary-operands left right)
    (cl:if (floating-value-p lhs)
           (build-fcmp (ensure-builder) float-predicate lhs rhs :name name :fast-math fast-math)
           (progn
             (check-integer-fast-math operation fast-math)
             (build-icmp (ensure-builder) int-predicate lhs rhs name)))))

(defun cmp (predicate left right &rest options)
  "Compare LEFT and RIGHT using an integer or floating-point PREDICATE."
  (destructuring-bind (&key (name "") fast-math) (instruction-name-options options)
    (compare-ir-operands 'cmp left right predicate predicate name fast-math)))

(defun = (left right &rest options)
  (destructuring-bind (&key (name "") fast-math) (instruction-name-options options)
    (compare-ir-operands '= left right :oeq :eq name fast-math)))

(defun /= (left right &rest options)
  (destructuring-bind (&key (name "") fast-math) (instruction-name-options options)
    (compare-ir-operands '/= left right :une :ne name fast-math)))

(defun < (left right &key (signed t) (name "") fast-math)
  "Compare LEFT and RIGHT. Integer comparisons default to signed predicates.
Use :SIGNED NIL for unsigned comparisons."
  (compare-ir-operands '< left right :olt (cl:if signed :slt :ult) name fast-math))

(defun <= (left right &key (signed t) (name "") fast-math)
  "Compare LEFT and RIGHT using signed integer predicates by default."
  (compare-ir-operands '<= left right :ole (cl:if signed :sle :ule) name fast-math))

(defun > (left right &key (signed t) (name "") fast-math)
  "Compare LEFT and RIGHT using signed integer predicates by default."
  (compare-ir-operands '> left right :ogt (cl:if signed :sgt :ugt) name fast-math))

(defun >= (left right &key (signed t) (name "") fast-math)
  "Compare LEFT and RIGHT using signed integer predicates by default."
  (compare-ir-operands '>= left right :oge (cl:if signed :sge :uge) name fast-math))

(defun ensure-condition-i1 (condition)
  (cl:cond
    ((value-p condition)
     (let ((type (value-type condition)))
       (cl:unless (eq (type-kind type) :integer)
         (error "Expected an integer LLVM condition, got ~A." (type-ir type)))
       (cl:if (cl:= (integer-width type) 1) condition (/= condition 0))))
    ((typep condition '(cl:or boolean integer))
     (const :i1 (cl:and condition (cl:not (eql condition 0)))
            (builder-context (ensure-builder))))
    (t (error "Expected an integer or boolean LLVM condition, got ~S." condition))))

(defun cast (operation value type-spec &optional (name ""))
  (build-cast (ensure-builder) operation (ensure-value value) type-spec name))

(defun int-cast (value type-spec &key signed (name ""))
  (build-int-cast (ensure-builder) (ensure-value value) type-spec :signed signed :name name))

(defun select (test yes no &optional (name ""))
  (multiple-value-bind (lhs rhs) (coerce-binary-operands yes no)
    (build-select (ensure-builder)
                  (cl:if (cl:and (value-p test)
                                 (member (type-kind (value-type test)) '(:vector :scalable-vector)))
                         test
                         (ensure-condition-i1 test))
                  lhs rhs name)))

(defun extract (aggregate index &rest more-indices)
  "Extract an element or nested field chain from AGGREGATE."
  (let ((builder (ensure-builder))
        (current aggregate))
    (dolist (idx (cons index more-indices) current)
      (setf current (build-extract-value builder current idx)))))

(defun insert (aggregate value index &optional (name ""))
  (let* ((builder (ensure-builder))
         (operand (cl:or (operand-value value)
                         (let* ((type (value-type aggregate))
                                (element (case (type-kind type)
                                           (:array (type-subtype type 0))
                                           (:struct (type-subtype type index)))))
                           (cl:unless element
                             (error "Cannot insert into field ~D of LLVM type ~A."
                                    index (type-ir type)))
                           (const element value)))))
    (build-insert-value builder aggregate operand index name)))

(defun vector-index (builder index)
  (cl:if (value-p index) index (const :i32 index (builder-context builder))))

(defun extract-element (vector index &optional (name ""))
  "Extract a lane from VECTOR using an LLVM integer or an i32 literal index."
  (let ((builder (ensure-builder)))
    (build-extract-element builder (ensure-value vector) (vector-index builder index) name)))

(defun insert-element (vector value index &optional (name ""))
  "Return VECTOR with the specified lane replaced."
  (let* ((builder (ensure-builder))
         (vector (ensure-value vector))
         (element-type (type-subtype (value-type vector) 0)))
    (build-insert-element builder vector (ensure-value value element-type)
                          (vector-index builder index) name)))

(defun shuffle-vector (left right mask &optional (name ""))
  "Shuffle two LLVM vectors using a constant i32 mask or a sequence of lane indices.
Indices address LEFT followed by RIGHT. -1 denotes a poison result lane."
  (multiple-value-bind (left right) (coerce-binary-operands left right)
    (let* ((builder (ensure-builder))
           (mask (cl:or (operand-value mask)
                        (let ((indices (sequence->list mask))
                              (element-type (int-type (builder-context builder) 32)))
                          (const-vector
                           (vector-type element-type (length indices))
                           (mapcar (lambda (index)
                                     (cl:if (eql index -1) (const-poison element-type) index))
                                   indices))))))
      (build-shuffle-vector builder left right mask name))))

(defun freeze (value &optional (name ""))
  "Emit LLVM freeze for VALUE."
  (build-freeze (ensure-builder) (ensure-value value) name))

(defun defglobal (name storage-type &key (module *module*) (initializer nil initializer-p)
                                        constant (linkage :external) address-space)
  "Define or declare a global in MODULE."
  (apply #'add-global (ensure-module module) (c-symbol-name name) storage-type
         :constant constant :linkage linkage :address-space address-space
         (cl:when initializer-p (list :initializer initializer))))

(defun function-return-type (&optional (function (ensure-function)))
  "Return the LLVM result type of FUNCTION, defaulting to the current IR function."
  (type-subtype (function-signature function) 0))

(defun call-with-ir-body (body)
  (let* ((tag (gensym "IR-PATH"))
         (*ir-body-exit* (cons *builder* tag)))
    (catch tag (funcall body))))

(defun finish-ir-path ()
  (cl:when (cl:and *ir-body-exit* (eq (car *ir-body-exit*) *builder*))
    (throw (cdr *ir-body-exit*) (values)))
  (values))

(defun ret (&optional (value nil value-supplied-p))
  "Emit a return instruction in *BUILDER*, coercing literal VALUE to *FUNCTION*'s return type."
  (let ((builder (ensure-builder)))
    (cl:if value-supplied-p
           (let ((type (function-return-type)))
             (cl:when (eq (type-kind type) :void)
               (error "A void LLVM function cannot return a value."))
             (build-ret builder (ensure-value value type)))
           (build-ret builder))
    (finish-ir-path)))

(defun unreachable ()
  (build-unreachable (ensure-builder))
  (finish-ir-path))

(defun configure-ir-function (function convention attributes)
  (cl:when convention
    (set-function-calling-convention function convention))
  (dolist (attribute attributes)
    (apply #'set-function-attribute function attribute))
  function)

(defmacro declare-ir (name parameters result &key (module '*module*) variadic linkage
                                                  calling-convention attributes)
  "Declare a function in MODULE. ATTRIBUTES evaluates to (location name &optional value) lists."
  `(configure-ir-function
    (declare-function (ensure-module ,module) ,(c-symbol-name name) ,result (list ,@parameters)
                      :variadic ,variadic :linkage ,linkage)
    ,calling-convention ,attributes))

(defun emit-implicit-return (function builder body-values)
  (let ((block (builder-block builder)))
    (cl:unless block
      (error "LLVM function ~A has no insertion block for its implicit return."
             (function-name function)))
    (cl:unless (block-terminated-p block)
      (let ((type (function-return-type function)))
        (cl:cond
          ((eq (type-kind type) :void) (build-ret builder))
          (body-values (build-ret builder (ensure-value (first body-values) type)))
          (t (error "LLVM function ~A needs a return value of type ~A."
                    (function-name function) (type-ir type))))))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun ir-result-forms (type body)
    (let ((tail (car (last body))))
      (cl:if (cl:and body (cl:not (cl:and (consp tail) (eq (car tail) 'declare))))
             `(,@(butlast body) (ir-result-form ,type ,tail))
             body))))

(defmacro ir-result-form (type form &environment environment)
  ;; Follow Lisp macro expansions and tail positions without rewriting operands.
  (let ((form (macroexpand form environment)))
    (cl:if (consp form)
           (case (first form)
             ((emit-if emit-cond emit-switch) `(,@form (lambda () ,type)))
             ((progn locally) `(,(first form) ,@(ir-result-forms type (rest form))))
             ((let let* flet labels macrolet symbol-macrolet block)
              `(,(first form) ,(second form) ,@(ir-result-forms type (cddr form))))
             (cl:if
              (destructuring-bind (test then &optional else) (rest form)
                `(cl:if ,test (ir-result-form ,type ,then) (ir-result-form ,type ,else))))
             (otherwise form))
           form)))

(defmacro defir (name-and-options parameters result &body body)
  "Define an LLVM function. An open final block returns the last body value.
The declared result type coerces a final literal and types IF, COND, and
SWITCH in result position."
  (destructuring-bind (name &key (module '*module*) variadic linkage calling-convention attributes)
      (cl:if (consp name-and-options) name-and-options (list name-and-options))
    (let ((fn (gensym "FUNCTION"))
          (builder (gensym "BUILDER")))
      `(define-function (,fn ,builder (ensure-module ,module) ,(c-symbol-name name) ,result
                         :variadic ,variadic :linkage ,linkage)
           ,parameters
         (configure-ir-function ,fn ,calling-convention ,attributes)
         (emit-implicit-return ,fn ,builder
           (multiple-value-list
            (call-with-ir-body
             (lambda ()
               ,@(ir-result-forms `(function-return-type ,fn)
                                  (cl:or body '((values))))))))))))

(defmacro ir-arm-emitter (&body body)
  (let ((type (gensym "ARM-TYPE")))
    `(lambda (,type)
       (declare (ignorable ,type))
       ,@(ir-result-forms type body))))

(defun emit-ir-arm (builder entry emit &optional type)
  (position-at-end builder entry)
  (let* ((result (cl:when emit
                   (multiple-value-list
                    (call-with-ir-body
                     (lambda () (funcall emit (cl:unless (eq type :void) type)))))))
         (end (builder-block builder)))
    (cl:unless end
      (error "An LLVM branch arm must finish with an insertion block."))
    (cl:unless (block-terminated-p end)
      (cl:if result (list end (first result)) (list end)))))

(defun merge-ir-values (builder incoming type name)
  (cl:cond
    ((null incoming) (values))
    ((null (rest incoming)) (cdar incoming))
    (t
     (let ((phi (build-phi builder type (format nil "~A.val" name))))
       (dolist (edge incoming phi)
         (add-incoming phi (cdr edge) (car edge)))))))

(defun merge-ir-arms (builder function arms type name)
  (cl:unless arms
    (return-from merge-ir-arms (finish-ir-path)))
  (let* ((result-type
           (cl:cond
             ((eq type :void) nil)
             ((every #'cdr arms)
              (cl:or type (infer-literal-type (mapcar #'second arms))
                     (error "LLVM ~A arms yield only literals. Supply :TYPE or return a typed LLVM value in an arm."
                            name)))
             ((some #'cdr arms)
              (error "LLVM ~A has continuing arms with and without values. Use :TYPE :VOID to discard arm results."
                     name))
             (type
              (error "Each continuing LLVM ~A arm must yield a value of type ~A."
                     name (type-ir type)))))
         (incoming
           (cl:when result-type
             (cl:loop for (block value) in arms
                      for operand = (ensure-value value result-type)
                      for actual = (value-type operand)
                      do (cl:unless (type-equal-p result-type actual)
                           (error "LLVM ~A arm has type ~A, expected ~A."
                                  name (type-ir actual) (type-ir result-type)))
                      collect (cons block operand)))))
    (cl:if (rest arms)
           (let ((merge (append-block function (format nil "~A.merge" name))))
             (dolist (arm arms)
               (position-at-end builder (first arm))
               (build-br builder merge))
             (position-at-end builder merge))
           ;; Clear the arm's debug location before emitting the continuation.
           (position-at-end builder (first (first arms))))
    (merge-ir-values builder incoming result-type name)))

(defun structured-result-type (builder explicit-type expected-type &optional implicit-fallthrough-p)
  (cl:cond
    (explicit-type
     (let ((type (resolve-type explicit-type (builder-context builder))))
       (cl:if (eq (type-kind type) :void) :void type)))
    (implicit-fallthrough-p :void)
    (expected-type
     (let ((type (funcall expected-type)))
       (cl:when (cl:and type (cl:not (eq (type-kind type) :void)))
         type)))))

(defun condition-result (result operator)
  (cl:unless result
    (error "An LLVM ~A test must yield an integer or boolean value." operator))
  (ensure-condition-i1 (first result)))

(defun emit-if (test-fn then-fn else-fn type-fn &optional expected-type)
  (let* ((builder (ensure-builder))
         (function (ensure-function))
         (condition (multiple-value-list (call-with-ir-body test-fn))))
    (cl:when (block-terminated-p (builder-block builder))
      (return-from emit-if (finish-ir-path)))
    (let* ((type-spec (funcall type-fn))
           (test (condition-result condition "IF"))
           (type (structured-result-type builder type-spec expected-type (null else-fn)))
           (then-block (append-block function "then"))
           (else-block (append-block function "else")))
      (build-cond-br builder test then-block else-block)
      (let* ((then-arm (emit-ir-arm builder then-block then-fn type))
             (else-arm (emit-ir-arm builder else-block else-fn type)))
        (merge-ir-arms builder function (remove nil (list then-arm else-arm)) type "if")))))

(defmacro if (condition then-form &rest options)
  "Emit a conditional. Continuing arms that yield values produce one SSA result.
Place :TYPE after the then form or optional else form. An omitted else arm
gives a statement by default. Use :TYPE :VOID to discard arm results explicitly.
A terminated test emits no arms."
  (let* ((else-p (oddp (length options)))
         (else-form (cl:when else-p (pop options))))
    (destructuring-bind (&key type) options
      `(emit-if (lambda () ,condition) (ir-arm-emitter ,then-form)
                ,(cl:when else-p `(ir-arm-emitter ,else-form)) (lambda () ,type)))))

(defmacro when (condition &body body)
  "Emit BODY when CONDITION is true. This form yields no IR value."
  `(if ,condition (locally ,@body) :type :void))

(defmacro unless (condition &body body)
  "Emit BODY when CONDITION is false. This form yields no IR value."
  `(if ,condition (values) (locally ,@body) :type :void))

(defun ir-clause-type (clauses)
  (zk:match (first clauses)
    ((list :type spec) (values (rest clauses) spec))
    (_ (values clauses nil))))

(defun emit-cond (clauses type-spec &optional expected-type)
  (let* ((builder (ensure-builder))
         (function (ensure-function))
         (type (structured-result-type builder type-spec expected-type))
         (arms nil))
    (cl:loop for (test-fn . body-fn) in clauses
          do (cl:unless test-fn
               (let ((arm (emit-ir-arm builder (builder-block builder) body-fn type)))
                 (cl:when arm (push arm arms)))
               (return))
             (let ((test-result (multiple-value-list (call-with-ir-body test-fn))))
               (cl:when (block-terminated-p (builder-block builder))
                 (return))
               (let* ((test (condition-result test-result "COND"))
                      (yes (append-block function "cond.then"))
                      (no (append-block function "cond.next")))
                 (build-cond-br builder test yes no)
                 (let ((arm (emit-ir-arm builder yes
                                         (cl:or body-fn
                                                (lambda (type)
                                                  (declare (ignore type))
                                                  (first test-result)))
                                         type)))
                   (cl:when arm (push arm arms)))
                 (position-at-end builder no)))
          finally
             (push (list (builder-block builder)) arms)
             (cl:unless type-spec (setf type :void)))
    (merge-ir-arms builder function (nreverse arms) type "cond")))

(defmacro cond (&rest clauses)
  "Emit a multi-arm conditional. Supply an initial (:TYPE SPEC) for all-literal results.
An initial (:TYPE :VOID) or a fallthrough with no default clause discards results by default."
  (multiple-value-bind (clauses type) (ir-clause-type clauses)
    (cl:if (null clauses)
           '(values)
           `(emit-cond
             (list
              ,@(cl:loop for ((test . body) . rest) on clauses
                      for default-p = (member test '(t :else :otherwise))
                      do (cl:when (cl:and default-p rest)
                           (error "The default LLVM COND clause must be last."))
                      collect `(cons ,(cl:unless default-p `(lambda () ,test))
                                      ,(cl:when body `(ir-arm-emitter ,@body)))))
             ,type))))

(defun emit-while (test-fn body-fn)
  (let* ((builder (ensure-builder))
         (function (ensure-function))
         (cond-block (append-block function "while.cond"))
         (exit-block nil))
    (build-br builder cond-block)
    (position-at-end builder cond-block)
    (flet ((exit-target ()
             (cl:or exit-block (setf exit-block (append-block function "while.exit")))))
      (let* ((*loop-control*
               (make-ir-loop-control
                :builder builder
                :break (lambda (value value-p)
                         (declare (ignore value))
                         (cl:when value-p
                           (error "LLVM:BREAK in LLVM:WHILE takes no value."))
                         (build-br builder (exit-target)))
                :continue (lambda () (build-br builder cond-block))))
             (test-result (multiple-value-list (call-with-ir-body test-fn))))
        (cl:unless (block-terminated-p (builder-block builder))
          (let ((test (condition-result test-result "WHILE"))
                (body-block (append-block function "while.body")))
            (build-cond-br builder test body-block (exit-target))
            (cl:when (emit-ir-arm builder body-block body-fn)
              (build-br builder cond-block))))))
    (cl:if exit-block
           (position-at-end builder exit-block)
           (finish-ir-path)))
  (values))

(defmacro while (test-form &body body)
  "Emit a structured loop with BREAK and CONTINUE in BODY."
  `(emit-while (lambda () ,test-form)
               (ir-arm-emitter ,@body (values))))

(defun active-loop-control ()
  (cl:when (cl:and *loop-control* (eq (ir-loop-control-builder *loop-control*) *builder*))
    *loop-control*))

(defun break (&optional (value nil value-p))
  "Exit the innermost LLVM loop, supplying VALUE if it has a result type."
  (let ((control (active-loop-control)))
    (cl:unless control
      (error "LLVM:BREAK used outside an LLVM loop."))
    (funcall (ir-loop-control-break control) value value-p))
  (finish-ir-path))

(defun continue ()
  "Start the next iteration of the innermost LLVM loop with unchanged state."
  (let ((control (active-loop-control)))
    (cl:unless control
      (error "LLVM:CONTINUE used outside an LLVM loop."))
    (funcall (ir-loop-control-continue control)))
  (finish-ir-path))

(defun recur (&rest next-values)
  "Start the next iteration of LLVM:LOOP with NEXT-VALUES as the carried state."
  (let* ((control (active-loop-control))
         (emit (cl:when control (ir-loop-control-recur control))))
    (cl:unless emit
      (error "LLVM:RECUR requires an LLVM:LOOP as the innermost loop."))
    (funcall emit next-values))
  (finish-ir-path))

(defun initialize-loop-state (name type-spec initialize)
  (let ((type (resolve-type type-spec (builder-context (ensure-builder)))))
    (cl:when (eq (type-kind type) :void)
      (error "LLVM:LOOP state ~A must have a value type, got void." name))
    (let ((value (funcall initialize)))
      (cl:when (block-terminated-p (builder-block (ensure-builder)))
        (return-from initialize-loop-state (finish-ir-path)))
      (list name type (coerce-operand value type 'loop)))))

(defun emit-ssa-loop (initial-state result-type-spec body)
  (let* ((builder (ensure-builder))
         (function (ensure-function))
         (type (resolve-type result-type-spec (builder-context builder)))
         (result-type (cl:unless (eq (type-kind type) :void) type))
         (entry (builder-block builder))
         (header (append-block function "loop.header"))
         (exit nil)
         (incoming nil))
    (build-br builder header)
    (position-at-end builder header)
    (let ((state (cl:loop for (name type initial) in initial-state
                          collect (let ((phi (build-phi builder type name)))
                                    (add-incoming phi initial entry)
                                    phi))))
      (flet ((emit-recur (next-values)
               (cl:unless (cl:= (length next-values) (length state))
                 (error "LLVM:RECUR expects ~D carried values, got ~D."
                        (length state) (length next-values)))
               (let ((next (cl:loop for value in next-values
                                    for (nil type) in initial-state
                                    collect (coerce-operand value type 'recur)))
                     (predecessor (builder-block builder)))
                 (build-br builder header)
                 (mapc (lambda (phi value) (add-incoming phi value predecessor)) state next)))
             (emit-break (value value-p)
               (cl:cond
                 ((cl:and result-type (cl:not value-p))
                  (error "LLVM:BREAK requires a result of type ~A." (type-ir result-type)))
                 ((cl:and (null result-type) value-p)
                  (error "LLVM:BREAK in a void LLVM:LOOP takes no value.")))
               (let ((operand (cl:when result-type (coerce-operand value result-type 'break)))
                     (predecessor (builder-block builder)))
                 (cl:unless exit
                   (setf exit (append-block function "loop.exit")))
                 (build-br builder exit)
                 (cl:when result-type (push (cons predecessor operand) incoming)))))
        (let ((*loop-control* (make-ir-loop-control
                               :builder builder
                               :break #'emit-break
                               :continue (lambda () (emit-recur state))
                               :recur #'emit-recur)))
          (call-with-ir-body (lambda () (apply body state))))))
    (cl:unless (block-terminated-p (builder-block builder))
      (error "Each LLVM:LOOP path must end with RECUR, CONTINUE, BREAK, or another terminator."))
    (cl:if exit
           (progn
             (position-at-end builder exit)
             (merge-ir-values builder (nreverse incoming) result-type "loop"))
           (finish-ir-path))))

(defmacro loop (bindings result-type &body body)
  "Emit an SSA loop with (VARIABLE TYPE INITIAL) bindings and RESULT-TYPE.
INITIAL forms run once in order in the enclosing Lisp scope.
BODY receives the loop-carried SSA values.
RECUR supplies parallel updates and BREAK supplies the result unless it is :VOID."
  (let ((variables (mapcar #'first bindings)))
    (cl:unless (cl:= (length variables) (length (remove-duplicates variables)))
      (error "LLVM:LOOP state variables must be distinct: ~S." variables))
    `(emit-ssa-loop
      (list ,@(cl:loop for binding in bindings
                       collect (destructuring-bind (variable type initial) binding
                                 `(initialize-loop-state ,(string-downcase (symbol-name variable))
                                                         ,type (lambda () ,initial)))))
      ,result-type
      (lambda ,variables
        (declare (ignorable ,@variables))
        ,@body))))

(defun emit-switch (value-fn clauses type-fn &optional expected-type)
  (let* ((builder (ensure-builder))
         (function (ensure-function))
         (result (multiple-value-list (call-with-ir-body value-fn))))
    (cl:when (block-terminated-p (builder-block builder))
      (return-from emit-switch (finish-ir-path)))
    (cl:unless result
      (error "An LLVM SWITCH selector must yield an integer value."))
    (let* ((type-spec (funcall type-fn))
           (value (ensure-value (first result)))
           (value-type (value-type value))
           (default-p (assoc :default clauses))
           (type (structured-result-type builder type-spec expected-type (null default-p)))
           (default-block (append-block function "switch.default"))
           (cases nil)
           (emitters
             (cl:loop with index = 0
                      for (keys . emit) in clauses
                      for default-p = (eq keys :default)
                      for block = (cl:if default-p default-block
                                         (append-block function (format nil "switch.case.~D"
                                                                        (prog1 index (incf index)))))
                      unless default-p do (dolist (key keys)
                                            (push (list (ensure-value key value-type) block) cases))
                      collect (cons block emit))))
      (cl:unless default-p
        (setf emitters (nconc emitters (list (list default-block)))))
      (build-switch builder value default-block (nreverse cases))
      (merge-ir-arms builder function
                     (cl:loop for (block . emit) in emitters
                              for arm = (emit-ir-arm builder block emit type)
                              when arm collect arm)
                     type "switch"))))

(defmacro switch (value &body clauses)
  "Emit a multi-way branch with (KEY-OR-KEYS . BODY) and (:DEFAULT . BODY) clauses.
Supply an initial (:TYPE SPEC) for all-literal results.
An initial (:TYPE :VOID) or an omitted default clause discards clause results by default."
  (multiple-value-bind (clauses type) (ir-clause-type clauses)
    (let ((default-p nil))
      `(emit-switch (lambda () ,value)
                    (list ,@(cl:loop for (keys . body) in clauses
                                     for emit = `(ir-arm-emitter ,@(cl:or body '((values))))
                                     collect
                                       (cl:cond
                                         ((member keys '(:default :otherwise t))
                                          (cl:when default-p
                                            (error "LLVM SWITCH accepts one default clause."))
                                          (setf default-p t)
                                          `(cons :default ,emit))
                                         ((null keys)
                                          (error "Each LLVM SWITCH case needs at least one key."))
                                         (t `(cons ',(cl:if (listp keys) keys (list keys)) ,emit)))))
                    (lambda () ,type)))))

(defmacro jit-fn (name-and-options parameters result &body body)
  "Compile an IR function with a scalar C ABI. Return a callable and a zero-argument releaser.
The releaser unloads the compilation and invalidates the callable. Other live
lookup symbols, dependent code, or a call in progress block release. The caller
can retry after those uses end."
  (destructuring-bind (name &key (optimize :o2) jit)
      (cl:if (consp name-and-options) name-and-options (list name-and-options))
    (let ((c-name (c-symbol-name name))
          (module (gensym "MODULE"))
          (engine (gensym "JIT"))
          (function (gensym "FUNCTION"))
          (entry (gensym "ENTRY"))
          (result-type (gensym "RESULT-TYPE"))
          (parameter-types (gensym "PARAMETER-TYPES"))
          (pipeline (gensym "PIPELINE"))
          (parameter-bindings (cl:loop repeat (length parameters) collect (gensym "PARAMETER-TYPE"))))
      `(call-with-jit-compilation
        ,jit
        (lambda (,engine)
          (with-module (,module ,c-name :jit ,engine)
            (let* ((,result-type (resolve-jit-abi-type ,result t))
                   ,@(cl:loop for (nil type) in parameters for binding in parameter-bindings
                              collect `(,binding (resolve-jit-abi-type ,type)))
                   (,function
                     (defir ,c-name
                         ,(cl:loop for (variable) in parameters for binding in parameter-bindings
                                   collect `(,variable (jit-abi-type-ir ,binding)))
                         (jit-abi-type-ir ,result-type)
                       ,@body))
                   (,parameter-types (list ,@parameter-bindings))
                   (,entry (jit-entry-name ,module ,function ,result-type ,parameter-types))
                   (,pipeline ,optimize))
              (bind-jit-entry ,engine
                              (cl:if ,pipeline
                                     (optimize-module ,module (cl:if (eq ,pipeline t) :o2 ,pipeline))
                                     ,module)
                              ,entry ,result-type ,parameter-types))))))))
