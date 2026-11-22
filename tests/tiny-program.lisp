;;;; Standalone entry point for the Tiny compiler.
(require "asdf")
(asdf:load-system "zk-llvm/tiny")

(zk-llvm/examples/compiler:main)
