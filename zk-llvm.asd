(asdf:defsystem "zk-llvm"
  :description "High-level LLVM bindings for zkcl"
  :version "0.1.0"
  :license "Apache-2.0"
  :serial t
  :components ((:file "lisp/package")
               (:file "lisp/ffi")
               (:file "lisp/ir")
               (:file "lisp/symbols")
               (:file "lisp/aliases")
               (:file "lisp/instructions")
               (:file "lisp/calls")
               (:file "lisp/assembly")
               (:file "lisp/inspection")
               (:file "lisp/attributes")
               (:file "lisp/metadata")
               (:file "lisp/debug")
               (:file "lisp/target")
               (:file "lisp/module-files")
               (:file "lisp/jit")
               (:file "lisp/dsl")
               (:file "lisp/exception")
               (:file "lisp/memory"))
  :in-order-to ((asdf:test-op (asdf:test-op "zk-llvm/tests"))))

(asdf:defsystem "zk-llvm/tiny"
  :depends-on ("zk-llvm")
  :serial t
  :components ((:module "examples/tiny"
                :serial t
                :components ((:file "frontend")
                             (:file "interpreter")
                             (:file "llvm")
                             (:file "repl")
                             (:file "main")))))

(asdf:defsystem "zk-llvm/tests"
  :license "Apache-2.0"
  :depends-on ("zk-llvm" "zk-pbt")
  :serial t
  :components ((:file "examples/positive-part/builder")
               (:file "examples/funclet-ir")
               (:file "tests/llvm")
               (:file "tests/cfg")
               (:file "tests/advanced-ir")
               (:file "tests/control-flow")
               (:file "tests/function-body")
               (:file "tests/expression-contract")
               (:file "tests/jit-lifetime")
               (:file "tests/edit-emission")
               (:file "tests/debug-api")
               (:file "tests/literals")
               (:file "tests/memory")
               (:file "tests/aliases")
               (:file "tests/quickstart"))
  :perform (asdf:test-op (operation system)
             (declare (ignore operation system))
             (uiop:symbol-call :zk-llvm/tests :run-tests)))
