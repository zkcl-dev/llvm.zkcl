(in-package #:zk-llvm/tests)

(defun test-cfg-inspection ()
  (llvm:with-llvm-objects ()
    (let* ((context (llvm:make-context))
           (module (llvm:parse-ir context
                     "define i32 @flow(i32 %selector, i1 %again) {
                      entry:
                        %tag = add i32 %selector, 1
                        switch i32 %tag, label %loop [ i32 0, label %other
                                                       i32 1, label %loop ]
                      loop:
                        br i1 %again, label %loop, label %done
                      other:
                        br label %loop
                      done:
                        ret i32 %tag
                      dead:
                        unreachable
                      }"))
           (function (llvm:find-function module "flow"))
           (entry (llvm:find-block function "entry"))
           (looping (llvm:find-block function "loop"))
           (add (llvm:find-instruction function "tag"))
           (argument (llvm:instruction-operand add 0)))
      (assert (equal '("loop" "other" "loop")
                     (mapcar #'llvm:block-name (llvm:block-successors entry))))
      (assert (equal '("entry" "entry" "loop" "other")
                     (mapcar #'llvm:block-name (llvm:block-predecessors looping))))
      (assert (equal '("entry" "other" "loop" "done")
                     (mapcar #'llvm:block-name (llvm:function-reverse-postorder function))))
      (assert (llvm:function-equal-p function (llvm:value-argument-function argument)))
      (assert (eql 0 (llvm:value-argument-index argument)))
      (assert (eql 1 (llvm:value-argument-index (llvm:parameter function 1))))
      (assert (null (llvm:value-argument-index (llvm:instruction-operand add 1))))
      (assert (null (llvm:value-function argument)))
      (assert (zk:match (llvm:block-terminator entry)
                ((llvm:instruction-opcode :switch) t)
                (_ nil)))
      (assert (not (zk:match argument
                     ((llvm:instruction-opcode :switch) t)
                     (_ nil))))
      (assert (equal '("selector" 1)
                     (zk:match add
                       ((llvm:instruction :add
                          (list (and (llvm:value-kind :argument) left)
                                (and (llvm:value-kind :constant-int) right)))
                        (list (llvm:value-name left) (llvm:constant-integer right)))
                       (_ nil)))))))
