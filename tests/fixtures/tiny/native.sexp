(fn "wrapped_sum" ((a :i64) (b :i64)) :i64
  (+ a b))

(fn "wrapped_product" ((a :i64) (b :i64)) :i64
  (* a b))

(fn "signed_order" ((a :i64) (b :i64)) :i64
  (if (< a b) -1 (if (< b a) 1 0)))

(fn "nested_scope" ((x :i64) (y :i64)) :i64
  (let ((x :i64 (+ x y)))
    (let ((positive :i1 (< 0 x)))
      (let ((y :i64 (if positive (* x 2) (- 0 x))))
        (+ y x)))))

(fn "nested_fold" ((start :i64) (end :i64) (seed :i64)) :i64
  (fold (i start end) (sum seed)
    (let ((i :i64 (+ i 1)))
      (fold (j 0 3) (sum sum)
        (if (< j 1) (- sum i) (+ sum (* j i)))))))

(fn "fold_parity" ((start :i64) (end :i64) (seed :i64)) :i64
  (if (fold (i start end) (flag (call "is_negative" seed))
        (call "toggle_if_negative" flag i))
    1 0))

(fn "toggle_if_negative" ((flag :i1) (value :i64)) :i1
  (if (call "is_negative" value) (if flag :false :true) flag))

(fn "is_negative" ((value :i64)) :i1
  (< value 0))

(fn "argument_order" ((a :i64) (b :i64) (c :i64)) :i64
  (call "combine_arguments" c (- a b) b))

(fn "combine_arguments" ((a :i64) (b :i64) (c :i64)) :i64
  (- (+ (* a 3) (* b 5)) c))

(fn "least_i64" () :i64
  -9223372036854775808)
