# Tiny compiler

Run these commands from `llvm.zkcl` after [setting up LLVM](../../README.md#quick-start).

```sh
zkcl run --bin tiny
zkcl run --bin tiny -- examples/tiny/demo.sexp sum_squares -2 3
```

The second command prints `sum_squares(-2, 3) = 10`.

Start with [demo.sexp](demo.sexp), then follow the checked program through its implementation.

| File | Responsibility |
| --- | --- |
| [frontend.lisp](frontend.lisp) | Read forms, collect signatures, and produce a typed `checked-expression` tree. |
| [interpreter.lisp](interpreter.lisp) | Evaluate that tree with Lisp values as a reference for the generated code. |
| [llvm.lisp](llvm.lisp) | Lower types, declare every function, then emit each body with the structured LLVM builders. |
| [repl.lisp](repl.lisp) | Keep definitions in an ORC JIT and execute temporary expression wrappers. |
| [main.lisp](main.lisp) | Select the interpreter, JIT, or file output and compare both evaluators on the demo. |

`zk:defvariant` and `zk:match` keep the checked tree's constructors and consumers
explicit. Type checking happens before either evaluator runs. Both evaluators
represent lexical environments as association lists, where the first binding
for a name wins. Adding a binding at the front implements shadowing.
All signatures are collected before checking bodies, so calls can refer to later
definitions or participate in mutual recursion.

## Language and lowering

A definition has the form `(fn "name" ((parameter TYPE) ...) RESULT body)`.
Types are `:i64`, `:i1`, and recursively `(pair LEFT-TYPE RIGHT-TYPE)`.

| Expression | Meaning |
| --- | --- |
| Signed 64-bit integer, `:true`, `:false`, variable | Scalar literal or lexical reference. |
| `(+ a b)`, `(- a b)`, `(* a b)`, `(< a b)` | Integer arithmetic wraps modulo 2^64. Comparison uses signed order and returns `:i1`. |
| `(let ((name TYPE initial)) body)` | Evaluate `initial` in the outer scope, then evaluate `body` with the new binding. |
| `(if test consequent alternative)` | Require an `:i1` test and matching arm types, then evaluate the selected arm. |
| `(fold (index start end) (acc initial) body)` | Evaluate the bounds and seed once, then update `acc` for each signed index in `[start, end)`. Return the seed when `start >= end`. |
| `(pair a b)`, `(first value)`, `(second value)` | Construct or project a typed pair. |
| `(call "name" argument ...)` | Call a declared function with exactly its parameter types. |

`llvm:if` emits branch blocks and a PHI for the selected value, including pair
values. A nested branch may finish in a different block from the one where its
arm started, and the builder records the actual predecessor. `llvm:loop` creates
loop-carried PHIs for the index and accumulator. The fold body sees the current
index and accumulator, and its result supplies the next accumulator. A pair
accumulator remains one SSA aggregate, built with `insertvalue` and read with
`extractvalue`.

The `--emit-ir` command exposes the lowering before LLVM optimizes it.

```sh
zkcl run --bin tiny -- --emit-ir examples/tiny/demo.sexp /tmp/tiny.ll
```

The output includes `range_moments`, whose pair carries both the sum and sum of
squares. Compare its PHIs and aggregate operations with the source fold.
`--emit-assembly` and `--emit-object` use `O2`; both accept an optional target
triple after the output path. `--interpret SOURCE ENTRY [INTEGER ...]` evaluates
the same checked program without JIT execution. Command-line entries take and
return only `:i64`, matching the `int64_t` ABI for native callers.

## REPL submissions and replacement

```sh
zkcl run --bin tiny -- --repl examples/tiny/demo.sexp
```

Enter `(call "range_moments" -2 3)` to get `(0 10)`, or enter definitions and
expressions directly in the REPL.

```lisp
(fn "step" ((x :i64)) :i64 (+ x 1))
(fn "caller" ((x :i64)) :i64 (call "step" x))
(call "caller" 10)
(fn "step" ((x :i64)) :i64 (+ x 2))
(call "caller" 10)
```

The calls print `11` and `12`. A definition may span several lines, and each
complete input submission is checked before installation. Multiple forms in one
submission share the collected signatures. Replacing a name rechecks and
recompiles all retained definitions in a fresh JIT, so existing callers resolve
the replacement. A rejected submission leaves the previous definitions usable.

Expression wrappers use temporary ORC resources. To return booleans or nested
pairs across the host boundary, a wrapper writes scalar leaves into an `int64_t`
buffer, and Lisp reconstructs their shape. This avoids depending on a platform's
aggregate return ABI. Definitions remain available until replacement or REPL
exit, and expression wrappers are removed after their results are read.
