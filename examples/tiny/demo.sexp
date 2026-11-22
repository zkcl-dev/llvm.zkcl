;;;; Each function declares its parameters and result before its body.
;;;; :I64 arithmetic wraps at 64 bits. Signed < returns an :I1 boolean.

(fn "score" ((n :i64)) :i64
  (let ((n :i64 (+ n 1)))
    (if (call "negative" n) 0 (call "sum_to" n))))

(fn "negative" ((n :i64)) :i1
  (< n 0))

(fn "sum_to" ((n :i64)) :i64
  (if (< n 1) 0 (+ n (call "sum_to" (- n 1)))))

(fn "difference" ((a :i64) (b :i64)) :i64
  (- a b))

(fn "odd_flag" ((n :i64)) :i64
  (let ((odd :i1 (if (< n 0) (call "odd" (- 0 n)) (call "odd" n))))
    (if odd 1 0)))

(fn "odd" ((n :i64)) :i1
  (if (< n 1) :false (call "even" (- n 1))))

(fn "even" ((n :i64)) :i1
  (if (< n 1) :true (call "odd" (- n 1))))

;; Collect two moments in one pass over the half-open range [start, end).
;; Callers choose the value they need by projecting a field from the pair.
(fn "range_moments" ((start :i64) (end :i64)) (pair :i64 :i64)
  (fold (index start end) (moments (pair 0 0))
    (pair (+ (first moments) index)
          (+ (second moments) (* index index)))))

(fn "sum_range" ((start :i64) (end :i64)) :i64
  (first (call "range_moments" start end)))

(fn "sum_squares" ((start :i64) (end :i64)) :i64
  (second (call "range_moments" start end)))
