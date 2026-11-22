# Examples

From the repository root, run `./install-llvm --run run --bin NAME` with an
example name below. If `ZK_LLVM_LIBRARY` is already set, use `zkcl run --bin NAME`.
[LLVM setup](../README.md#quick-start)

- `first-jit` builds `max(x, 0)` with `jit-fn` and prints the results for `-7`
  and `8`. `explicit-ir` prints the blocks, branches, and PHI for the same
  function. [JIT](positive-part/jit.lisp) · [Builder](positive-part/builder.lisp)
- `dispatch-table` calls through a constant table of function pointers.
  `jit-reload` switches new calls to a replacement shipping plugin and unloads
  the old version after its last call. [Dispatch](dispatch-table.lisp) · [Reload](jit-reload.lisp)
- `exception-ir` prints a cleanup path with `invoke`, `landingpad`, and `resume`.
  `funclet-ir` attaches a Windows cleanup pad's `funclet` token to calls in the
  pad. [Landing pad](exception-ir.lisp) · [Funclet](funclet-ir.lisp)

## Inspect and edit IR

`ir-pipeline` lists functions, parameter uses, and block instructions. With an
output path it runs `default<O2>` and writes `.ll`, `.bc`, `.s`, or `.o` according
to the suffix. The final argument can set LLVM's pass pipeline.
[pipeline.lisp](ir-tools/pipeline.lisp)

```sh
out=$(mktemp -d)
./install-llvm --run run --bin ir-pipeline -- examples/ir-tools/positive-part.ll "$out/positive-part.bc"
./install-llvm --run run --bin ir-pipeline -- "$out/positive-part.bc"
```

`rewrite-ir` changes the floor from `0` to `10` by inserting a comparison and
`select` before the return. It calls both versions on the same inputs.
[rewrite.lisp](ir-tools/rewrite.lisp)

## Link a native object

```sh
out=$(mktemp -d)
./install-llvm --run run --bin native-object -- "$out/order.o"
cc examples/native-object/driver.c "$out/order.o" -o "$out/order"
"$out/order"
```

The object exports `order_total(const struct Order *)` with the target's struct
layout. The driver prints `Order total = 845 cents`. Inspect the debug info with
`llvm-dwarfdump --verify "$out/order.o"`, or pass a target triple after the output
path to emit for that target. [Emitter](native-object/emit.lisp) · [C driver](native-object/driver.c)

## Tiny compiler

```sh
./install-llvm --run run --bin tiny -- examples/tiny/demo.sexp sum_squares -2 3
./install-llvm --run run --bin tiny -- --repl examples/tiny/demo.sexp
```

The first command prints `sum_squares(-2, 3) = 10`. Tiny uses one typed syntax
tree for its interpreter and LLVM lowering. The guide follows a fold into an
SSA loop and shows how the REPL replaces a definition. [Tiny guide](tiny/README.md)
