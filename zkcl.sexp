(project "zk/llvm"
  (version "0.1.0")
  (license "Apache-2.0")
  (packages "ZK-LLVM")
  (asdf-alias "zk-llvm" (version "0.1.0"))
  (include "README.md" "install-llvm" "zk-llvm.asd"
           "examples/README.md" "examples/ir-tools/positive-part.ll"
           "examples/tiny/README.md" "examples/tiny/demo.sexp" "examples/native-object/driver.c"
           "tests/compiler-native.lisp" "tests/tiny-program.lisp"
           "tests/fixtures/tiny/native.sexp" "tests/fixtures/tiny/corpus.sexp"
           "tests/llvm.lisp" "tests/cfg.lisp" "tests/advanced-ir.lisp"
           "tests/control-flow.lisp" "tests/function-body.lisp"
           "tests/expression-contract.lisp" "tests/jit-lifetime.lisp" "tests/edit-emission.lisp"
           "tests/debug-api.lisp" "tests/literals.lisp" "tests/memory.lisp"
           "tests/aliases.lisp" "tests/quickstart.lisp")
  (library
    (files "lisp/package.lisp"
           "lisp/ffi.lisp"
           "lisp/ir.lisp"
           "lisp/symbols.lisp"
           "lisp/aliases.lisp"
           "lisp/instructions.lisp"
           "lisp/calls.lisp"
           "lisp/assembly.lisp"
           "lisp/inspection.lisp"
           "lisp/attributes.lisp"
           "lisp/metadata.lisp"
           "lisp/debug.lisp"
           "lisp/target.lisp"
           "lisp/module-files.lisp"
           "lisp/jit.lisp"
           "lisp/dsl.lisp"
           "lisp/exception.lisp"
           "lisp/memory.lisp"))
  (binary "first-jit"
    (files "examples/positive-part/jit.lisp")
    (entry "ZK-LLVM/EXAMPLES/FIRST-JIT:MAIN"))
  (binary "explicit-ir"
    (files "examples/positive-part/builder.lisp")
    (entry "ZK-LLVM/EXAMPLES/EXPLICIT-IR:MAIN"))
  (binary "ir-pipeline"
    (files "examples/ir-tools/pipeline.lisp")
    (entry "ZK-LLVM/EXAMPLES/IR-PIPELINE:MAIN"))
  (binary "rewrite-ir"
    (files "examples/ir-tools/rewrite.lisp")
    (entry "ZK-LLVM/EXAMPLES/REWRITE-IR:MAIN"))
  (binary "dispatch-table"
    (files "examples/dispatch-table.lisp")
    (entry "ZK-LLVM/EXAMPLES/DISPATCH-TABLE:MAIN"))
  (binary "jit-reload"
    (files "examples/jit-reload.lisp")
    (entry "ZK-LLVM/EXAMPLES/JIT-RELOAD:MAIN"))
  (binary "native-object"
    (files "examples/native-object/emit.lisp")
    (entry "ZK-LLVM/EXAMPLES/NATIVE-OBJECT:MAIN"))
  (binary "exception-ir"
    (files "examples/exception-ir.lisp")
    (entry "ZK-LLVM/EXAMPLES/EXCEPTION-IR:MAIN"))
  (binary "funclet-ir"
    (files "examples/funclet-ir.lisp")
    (entry "ZK-LLVM/EXAMPLES/FUNCLET-IR:MAIN"))
  (binary "tiny"
    (files "examples/tiny/frontend.lisp"
           "examples/tiny/interpreter.lisp"
           "examples/tiny/llvm.lisp"
           "examples/tiny/repl.lisp"
           "examples/tiny/main.lisp")
    (entry "ZK-LLVM/EXAMPLES/COMPILER:MAIN")))
