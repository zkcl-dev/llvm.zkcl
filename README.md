# llvm.zkcl

LLVM 21.1 bindings for building IR, emitting objects, and JIT compilation in
zkcl. The ASDF system and Lisp package are named `zk-llvm`.

## Quick start

Use Cargo and a `zkcl` build with native FFI, `zk-treelist`, and the `zk:view` pattern
operator. Run this from the repository root.

```sh
./install-llvm --run run --bin first-jit
```

The helper builds `libzkcl_llvm_api` and sets `ZK_LLVM_LIBRARY` for the command.
It fetches LLVM on x86-64 Linux or on Apple Silicon with Nix. To use an existing
LLVM 21.1 installation, pass `--prefix PATH` before `--run`.

For direct `zkcl` invocations, set `ZK_LLVM_LIBRARY` to the shared library's path.
Standalone ASDF sessions also need this checkout in their source registry.
[Examples](examples/README.md)

## Use from Lisp

Start a session with `./install-llvm --run repl`.

```lisp
(require :asdf)
(asdf:load-system "zk-llvm")

(multiple-value-bind (positive-part release)
    (zk-llvm:jit-fn positive-part ((x :i64)) :i64
      (zk-llvm:if (zk-llvm:> x 0) x 0))
  (unwind-protect (funcall positive-part -7)
    (funcall release)))
```

The call returns `0` before the compiled function is released. Use `defir`
inside `define-module` to define several functions together. The `build-*`
functions expose explicit blocks and instructions.

`call` and `indirect-call` use the fixed parameter types to convert Lisp
literals. For a variadic argument, provide an LLVM value with the type the
callee expects, for example `(zk-llvm:const :i32 7)` for a C `int`.

Use `with-jit-module` to submit a module for a scope. By default it compiles an
object before entering the body. With `:on-demand t`, ORC compiles the module
when a symbol is needed. Nest bindings inside the module scope so the lookup
symbols are released before their code is unloaded.

```lisp
(zk-llvm:with-jit-module (jit module :on-demand t)
  (zk-llvm:with-jit-binding (f jit "positive_part" :i64 :i64)
    (funcall f -7)))
```

## Project dependency

Add the dependency to `zkcl.sexp` and run `zkcl lock`.

```lisp
(dependency "llvm" (module "zk/llvm")
  (git "https://github.com/zkcl-dev/llvm.zkcl.git")
  (rev "3448ef3f81c344bb94a44cd210874b07d6b1b989"))
```

For a local checkout, replace the `git` and `rev` forms with `(path "../llvm.zkcl")`.

## Tests

```sh
./install-llvm --run test zk-llvm
./install-llvm --run run tests/compiler-native.lisp
```

## License

Copyright (c) 2026 kawayww

llvm.zkcl is licensed under the [Apache License, Version 2.0](LICENSE). The
external LLVM library is covered by the [LLVM Project license](https://llvm.org/docs/DeveloperPolicy.html#new-llvm-project-license-framework).
