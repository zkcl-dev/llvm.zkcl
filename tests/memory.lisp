(in-package #:zk-llvm/tests)

(defun memory-operations-module (jit)
  (llvm:define-module (module "memory" :jit jit)
                      (let ((bytes (llvm:add-global-string "A" :name "memory.bytes")))
                        (llvm:defir copy-bytes () :i32
                                    (let* ((buffer (llvm:alloca :i8 :count 4 :alignment 8 :name "buffer"))
                                           (shifted (llvm:gep :i8 buffer '(1) :nuw t :nusw t :name "shifted"))
                                           (end (llvm:gep-in-bounds :i8 buffer '(3) "end")))
                                      (assert (llvm:instruction-p (llvm:memset buffer 2 4 :destination-alignment 8)))
                                      (llvm:memcpy buffer bytes 2 :destination-alignment 8 :source-alignment 1)
                                      (llvm:memmove shifted buffer 2 :volatile t)
                                      (llvm:+ (llvm:int-cast (llvm:load :i8 buffer "first") :i32)
                                              (llvm:int-cast (llvm:load :i8 shifted :volatile t) :i32)
                                              (llvm:int-cast (llvm:load :i8 (llvm:gep :i8 buffer '(2))) :i32)
                                              (llvm:int-cast (llvm:load :i8 end) :i32)))))
                      (llvm:defir atomic-update ((input :i32)) :i32
                                  (let ((slot (llvm:alloca :i32 0 "slot" :alignment 4)))
                                    (llvm:store input slot :alignment 4 :ordering :release :sync-scope :single-thread)
                                    (let* ((old (llvm:atomic-rmw :add slot 2 :type :i32 :ordering :acq-rel
                                                                 :alignment 4 :volatile t :sync-scope :single-thread))
                                           (next (llvm:int-binop :add old 2 :nsw t :nuw t))
                                           (pair (llvm:cmpxchg slot next input :success-ordering :acq-rel
                                                               :failure-ordering :acquire :alignment 4 :volatile t
                                                               :sync-scope :single-thread))
                                           (previous (llvm:extract pair 0))
                                           (success (llvm:int-cast (llvm:extract pair 1) :i32)))
                                      (llvm:fence :seq-cst :sync-scope :single-thread)
                                      (llvm:udiv (llvm:shl (llvm:+ previous success
                                                                   (llvm:load :i32 slot :ordering :acquire
                                                                              :sync-scope :single-thread))
                                                           1 :nuw t)
                                                 2 :exact t))))
                      (let ((heap (llvm:defglobal 'memory-heap '(:array :i32 4) :initializer :zero :linkage :private))
                            (requested (llvm:defglobal 'memory-requested :i32 :initializer 0 :linkage :private))
                            (released (llvm:defglobal 'memory-released :i32 :initializer 0 :linkage :private)))
                        (llvm:defir allocate-bytes ((size :i32)) :ptr
                                    (llvm:store size requested)
                                    (llvm:global-address heap))
                        (llvm:defir release-bytes ((pointer :ptr)) :void
                                    (llvm:store (llvm:int-cast (llvm:= pointer heap) :i32) released))
                        (llvm:defir typed-heap () :i32
                                    (let* ((base (llvm:malloc 'allocate-bytes :i32 3))
                                           (last (llvm:gep-in-bounds :i32 base '(2)))
                                           (offset (llvm:int-cast (llvm:ptr-diff :i32 last base) :i32)))
                                      (llvm:store 41 last :i32)
                                      (let ((result (llvm:load :i32 last)))
                                        (assert (llvm:instruction-p (llvm:free 'release-bytes base)))
                                        (llvm:+ result offset (llvm:load :i32 requested) (llvm:load :i32 released))))))))

(defun test-global-string-encoding ()
  (llvm:with-objects ()
    (llvm:with-module (module "global-strings" :verify t)
      (let* ((text (concatenate 'string "雪" (string (code-char 0)) "B"))
             (octets (make-array 6 :element-type '(unsigned-byte 8)
                                   :initial-contents '(233 155 170 0 66 0)))
             (encoded (llvm:add-global-string text :name "encoded"))
             (binary (llvm:add-global-string octets :name "binary" :nul-terminated nil)))
        (assert (= 6 (llvm:array-length (llvm:global-storage-type encoded))))
        (assert (llvm:value-equal-p (llvm:global-initializer encoded)
                                    (llvm:global-initializer binary)))))))

(defun test-memory-operations ()
  (test-global-string-encoding)
  (llvm:with-objects ()
                     (llvm:with-jit (jit)
                                    (let* ((module (memory-operations-module jit))
                                           (ir (llvm:module-ir module)))
                                      (dolist (fragment '("private unnamed_addr constant [2 x i8]" "alloca i8, i64 4, align 8"
                                                          "@llvm.memset" "@llvm.memcpy" "@llvm.memmove" "load volatile i8"
                                                          "getelementptr nusw nuw" "getelementptr inbounds" "store atomic i32"
                                                          "atomicrmw volatile add" "cmpxchg volatile" "load atomic i32"
                                                          "fence syncscope(\"singlethread\") seq_cst" "add nuw nsw i32"
                                                          "udiv exact i32" "@allocate_bytes(i32 12)" "sdiv exact"))
                                        (assert (search fragment ir)))
                                      (llvm:jit-add-module jit module)
                                      (assert (= 132 (funcall (llvm:jit-bind jit 'copy-bytes :i32))))
                                      (assert (= 43 (funcall (llvm:jit-bind jit 'atomic-update :i32 :i32) 20)))
                                      (assert (= 56 (funcall (llvm:jit-bind jit 'typed-heap :i32))))))))
