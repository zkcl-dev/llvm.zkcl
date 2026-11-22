;;;; Each top-level pair contains a Tiny module's definitions and its ABI cases.
;;;; Cases are (entry-name arguments expected-result), with the first case for
;;;; each entry also run through the interpreter and JIT. All symbol names in
;;;; the emitted objects are unique across modules so they can link together.

;; Signed subtraction is non-associative, including at the wrap boundary.
(((fn "subtract_on_right" ((a :i64) (b :i64) (c :i64)) :i64
    (- a (- b c))))
 (("subtract_on_right" (20 7 3) 16)
  ("subtract_on_right" (-4 10 3) -11)
  ("subtract_on_right" (-9223372036854775808 1 0) 9223372036854775807)
  ("subtract_on_right" (9223372036854775807 -1 2) -9223372036854775806)))

;; Independent expectation: c * (a - b) in the ring of 64-bit integers.
(((fn "mixed_polynomial" ((a :i64) (b :i64) (c :i64)) :i64
    (- (* a (+ b c)) (* (+ c a) b))))
 (("mixed_polynomial" (5 2 3) 9)
  ("mixed_polynomial" (-4 5 2) -18)
  ("mixed_polynomial" (9223372036854775807 -1 3) -9223372036854775808)
  ("mixed_polynomial" (0 -9223372036854775808 -1) -9223372036854775808)))

(((fn "branch_operands" ((x :i64) (y :i64)) :i64
    (- (* (if (< x y) x y) 3) (if (< x y) y x))))
 (("branch_operands" (-2 5) -11)
  ("branch_operands" (5 -2) -11)
  ("branch_operands" (4 4) 8)
  ("branch_operands" (-9223372036854775808 9223372036854775807) 1)))

;; An i1 join selects between two i64 joins, each with different computation.
(((fn "nested_branch_joins" ((a :i64) (b :i64) (c :i64)) :i64
    (if (if (< a b) (< b c) (< c a))
        (if (< a c) (+ a c) (- a c))
        (if (< a c) (* b c) (- b c)))))
 (("nested_branch_joins" (1 3 5) 6)
  ("nested_branch_joins" (5 3 2) 3)
  ("nested_branch_joins" (2 5 3) 15)
  ("nested_branch_joins" (4 4 4) 0)
  ("nested_branch_joins" (-7 -3 -1) -8)
  ("nested_branch_joins" (0 1 -1) 2)))

(((fn "shadow_with_type_change" ((x :i64) (y :i64)) :i64
    (let ((x :i1 (< x y)))
      (let ((x :i64 (if x 17 -4)))
        (+ (* x 5) y)))))
 (("shadow_with_type_change" (-2 0) 85)
  ("shadow_with_type_change" (2 0) -20)
  ("shadow_with_type_change" (5 5) -15)
  ("shadow_with_type_change" (-9223372036854775808 9223372036854775807)
    -9223372036854775724)))

;; The loop sums even numbers. With n added and the seed removed, it returns n²
;; for nonnegative n, and n itself when the loop is empty.
(((fn "fold_then_bind" ((n :i64) (seed :i64)) :i64
    (let ((sum :i64 (fold (i 0 n) (sum seed) (+ sum (* 2 i)))))
      (let ((sum :i64 (+ sum n)))
        (- sum seed)))))
 (("fold_then_bind" (6 8) 36)
  ("fold_then_bind" (0 9223372036854775807) 0)
  ("fold_then_bind" (-4 8) -4)
  ("fold_then_bind" (3 9223372036854775807) 9)))

;; The bounds and seed refer to outer bindings. The body sees both loop locals.
(((fn "fold_binding_scope" ((i :i64) (total :i64)) :i64
    (fold (i i (+ i 3)) (total (+ total i))
      (+ total i))))
 (("fold_binding_scope" (1 10) 17)
  ("fold_binding_scope" (-3 20) 11)
  ("fold_binding_scope" (0 0) 3)
  ("fold_binding_scope" (9223372036854775807 11) -9223372036854775798)
  ("fold_binding_scope" (-9223372036854775808 0) 3)))

;; Two loops produce values used by a third loop's bound and initial value.
(((fn "folds_in_preheader" ((a :i64) (b :i64)) :i64
    (fold (i 0 (fold (j 0 a) (bound 0) (+ bound 2)))
        (sum (fold (j 0 b) (seed 1) (* seed 2)))
      (+ sum i))))
 (("folds_in_preheader" (3 4) 31)
  ("folds_in_preheader" (0 3) 8)
  ("folds_in_preheader" (-4 -1) 1)
  ("folds_in_preheader" (2 0) 7)
  ("folds_in_preheader" (1 6) 65)))

;; For n >= 0 the accumulated difference is n * (n - 1) * (n + 1) / 6.
(((fn "fold_dependent_bounds" ((n :i64) (seed :i64)) :i64
    (fold (i 0 n) (sum seed)
      (fold (j 0 i) (sum sum)
        (+ sum (- i j))))))
 (("fold_dependent_bounds" (4 10) 20)
  ("fold_dependent_bounds" (1 -2) -2)
  ("fold_dependent_bounds" (0 17) 17)
  ("fold_dependent_bounds" (5 9223372036854775807) -9223372036854775789)))

(((fn "fold_boolean_state" ((start :i64) (end :i64) (seed :i64)) :i64
    (if (fold (i start end) (flag (< seed 0))
          (if (< i 0) (if flag :false :true) (if (< i 2) :true flag)))
        99 -99)))
 (("fold_boolean_state" (-5 -1 0) -99)
  ("fold_boolean_state" (-4 -1 0) 99)
  ("fold_boolean_state" (-4 -1 -1) -99)
  ("fold_boolean_state" (-5 3 0) 99)
  ("fold_boolean_state" (2 2 -1) 99)
  ("fold_boolean_state" (5 2 0) -99)))

;; An empty fold uses the initial predicate. A nonempty one uses the last index.
(((fn "fold_last_predicate" ((start :i64) (end :i64) (threshold :i64)) :i64
    (let ((state :i1 (fold (i start end) (state (if (< start end) :false :true))
                       (< i threshold))))
      (if state 1 0))))
 (("fold_last_predicate" (-2 3 2) 0)
  ("fold_last_predicate" (2 2 0) 1)
  ("fold_last_predicate" (8 4 0) 1)
  ("fold_last_predicate" (-2 3 3) 1)
  ("fold_last_predicate" (9223372036854775806 9223372036854775807 9223372036854775806) 0)))

(((fn "fold_horner" ((n :i64) (base :i64) (seed :i64)) :i64
    (fold (i 0 n) (value seed)
      (+ (* value base) i))))
 (("fold_horner" (4 2 1) 27)
  ("fold_horner" (3 -2 3) -24)
  ("fold_horner" (3 0 -9223372036854775808) 2)
  ("fold_horner" (2 2 9223372036854775807) -3)
  ("fold_horner" (-3 9 19) 19)))

;; Each branch exits a different inner loop before the outer loop continues.
(((fn "fold_chooses_loop" ((start :i64) (end :i64) (seed :i64)) :i64
    (fold (i start end) (sum seed)
      (if (< i 0)
          (fold (j i 0) (sum sum) (- sum j))
          (fold (j 0 i) (sum sum) (+ sum j))))))
 (("fold_chooses_loop" (-2 3 8) 13)
  ("fold_chooses_loop" (-3 0 -5) 5)
  ("fold_chooses_loop" (0 4 0) 4)
  ("fold_chooses_loop" (3 -2 9223372036854775807) 9223372036854775807)))

(((fn "call_mixed_arguments" ((a :i64) (b :i64) (c :i64)) :i64
    (call "select_mixed_arguments" (< a b) a (< b c) b c (< c a)))
  (fn "select_mixed_arguments" ((p :i1) (a :i64) (q :i1) (b :i64) (c :i64) (r :i1)) :i64
    (+ (if p a b) (* (if q b c) (if r 3 -2)))))
 (("call_mixed_arguments" (-2 4 8) -10)
  ("call_mixed_arguments" (6 2 4) 8)
  ("call_mixed_arguments" (3 7 1) 6)
  ("call_mixed_arguments" (5 5 5) -5)
  ("call_mixed_arguments" (-9223372036854775808 9223372036854775807 0) -9223372036854775808)))

;; Both sides share a callee. The result is a² + b² + a - b + 2.
(((fn "call_diamond" ((a :i64) (b :i64)) :i64
    (+ (call "call_diamond_left" a b) (call "call_diamond_right" a b)))
  (fn "call_diamond_left" ((a :i64) (b :i64)) :i64
    (- (call "call_diamond_leaf" a) b))
  (fn "call_diamond_leaf" ((x :i64)) :i64
    (+ (* x x) 1))
  (fn "call_diamond_right" ((a :i64) (b :i64)) :i64
    (+ (call "call_diamond_leaf" b) a)))
 (("call_diamond" (3 2) 16)
  ("call_diamond" (-2 3) 10)
  ("call_diamond" (0 0) 2)
  ("call_diamond" (-9223372036854775808 9223372036854775807) 4)))

(((fn "call_nullary_helpers" ((x :i64)) :i64
    (if (< x (call "nullary_zero"))
        (call "nullary_negative")
        (if (call "nullary_truth") (call "nullary_positive") x)))
  (fn "nullary_zero" () :i64 0)
  (fn "nullary_negative" () :i64 -17)
  (fn "nullary_truth" () :i1 :true)
  (fn "nullary_positive" () :i64 23))
 (("call_nullary_helpers" (-1) -17)
  ("call_nullary_helpers" (0) 23)
  ("call_nullary_helpers" (9223372036854775807) 23)
  ("call_nullary_helpers" (-9223372036854775808) -17)))

;; Twelve integer arguments span register and stack slots on x86-64 and AArch64.
(((fn "wide_abi" ((a :i64) (b :i64) (c :i64) (d :i64) (e :i64) (f :i64)
                  (g :i64) (h :i64) (i :i64) (j :i64) (k :i64) (l :i64)) :i64
    (call "wide_abi_weighted" l k j i h g f e d c b a))
  (fn "wide_abi_weighted" ((a :i64) (b :i64) (c :i64) (d :i64) (e :i64) (f :i64)
                           (g :i64) (h :i64) (i :i64) (j :i64) (k :i64) (l :i64)) :i64
    (+ (+ (+ (+ a (* b 2)) (+ (* c 3) (* d 4))) (+ (* e 5) (* f 6)))
       (+ (+ (+ (* g 7) (* h 8)) (+ (* i 9) (* j 10))) (+ (* k 11) (* l 12))))))
 (("wide_abi" (1 2 3 4 5 6 7 8 9 10 11 12) 364)
  ("wide_abi" (12 11 10 9 8 7 6 5 4 3 2 1) 650)
  ("wide_abi" (1 0 0 0 0 0 0 0 0 0 0 3) 15)
  ("wide_abi" (0 0 0 0 0 0 0 -2 3 0 0 0) 2)
  ("wide_abi" (1 0 0 0 0 0 0 0 0 0 0 9223372036854775807) -9223372036854775797)))

(((fn "branch_after_wrap" ((value :i64) (delta :i64)) :i64
    (let ((next :i64 (+ value delta)))
      (if (< next value) (- next value) (if (< value next) (+ delta 1) 0)))))
 (("branch_after_wrap" (9223372036854775807 1) 1)
  ("branch_after_wrap" (-9223372036854775808 -1) 0)
  ("branch_after_wrap" (7 -3) -3)
  ("branch_after_wrap" (-3 7) 8)
  ("branch_after_wrap" (-4 0) 0)
  ("branch_after_wrap" (0 9223372036854775807) -9223372036854775808)))

(((fn "recursive_fibonacci" ((n :i64)) :i64
    (if (< n 2) n
        (+ (call "recursive_fibonacci" (- n 1)) (call "recursive_fibonacci" (- n 2))))))
 (("recursive_fibonacci" (8) 21)
  ("recursive_fibonacci" (2) 1)
  ("recursive_fibonacci" (1) 1)
  ("recursive_fibonacci" (0) 0)
  ("recursive_fibonacci" (11) 89)
  ("recursive_fibonacci" (-5) -5)))

(((fn "mutual_recursion_state" ((n :i64) (sum :i64)) :i64
    (if (< n 1) sum (call "mutual_recursion_other" (- n 1) (+ sum n))))
  (fn "mutual_recursion_other" ((n :i64) (sum :i64)) :i64
    (if (< n 1) sum (call "mutual_recursion_state" (- n 1) (- sum (* 2 n))))))
 (("mutual_recursion_state" (5 9) 6)
  ("mutual_recursion_state" (6 0) -6)
  ("mutual_recursion_state" (3 -2) -2)
  ("mutual_recursion_state" (2 9223372036854775807) 9223372036854775807)
  ("mutual_recursion_state" (-1 3) 3)))

(((fn "recursive_euclid" ((a :i64) (b :i64)) :i64
    (if (< a 1) b
        (if (< b 1) a
            (if (< a b)
                (call "recursive_euclid" a (- b a))
                (call "recursive_euclid" (- a b) b))))))
 (("recursive_euclid" (48 18) 6)
  ("recursive_euclid" (18 48) 6)
  ("recursive_euclid" (21 21) 21)
  ("recursive_euclid" (0 15) 15)
  ("recursive_euclid" (27 0) 27)
  ("recursive_euclid" (7 12) 1)))

;; String function names and escaped lexical identifiers retain case.
(((fn "case_sensitive_names" ((x :i64) (|x| :i64)) :i64
    (+ (call "case_Sensitive_Value" x) (call "case_sensitive_Value" |x|)))
  (fn "case_Sensitive_Value" ((value :i64)) :i64 (+ value 2))
  (fn "case_sensitive_Value" ((value :i64)) :i64 (* value 3)))
 (("case_sensitive_names" (4 7) 27)
  ("case_sensitive_names" (7 4) 21)
  ("case_sensitive_names" (0 0) 2)
  ("case_sensitive_names" (9223372036854775807 1) -9223372036854775804)))

(((fn "fold_types_and_capture" ((value :i64) (amount :i64)) :i64
    (let ((start :i1 (< amount 0)))
      (if (fold (i 0 value) (flag start)
            (let ((start :i64 (if start (- 0 amount) amount)))
              (if (< i start) (if flag :false :true) flag)))
          7 -7))))
 (("fold_types_and_capture" (6 -3) -7)
  ("fold_types_and_capture" (2 -3) 7)
  ("fold_types_and_capture" (5 3) 7)
  ("fold_types_and_capture" (4 -9223372036854775808) 7)
  ("fold_types_and_capture" (-5 3) -7)))

(((fn "fold_limits" ((start :i64) (end :i64) (seed :i64)) :i64
    (fold (index start end) (result seed)
      (+ result (if (< index 0) (- index 1) (+ index 1))))))
 (("fold_limits" (9223372036854775805 9223372036854775807 0) -3)
  ("fold_limits" (-9223372036854775808 -9223372036854775806 4) 3)
  ("fold_limits" (9223372036854775807 9223372036854775807 17) 17)
  ("fold_limits" (-9223372036854775806 -9223372036854775808 -4) -4)
  ("fold_limits" (-2 3 0) 1)))

;; The nested pair flows through a function call, a branch join, and a loop.
;; Follow the last index's sign to select its corresponding accumulated sum.
(((fn "pair_fold" ((start :i64) (end :i64) (base :i64)) :i64
    (let ((state (pair :i1 (pair :i64 :i64))
                 (fold (i start end) (state (pair :false (pair 0 base)))
                   (call "pair_fold_update" state i))))
      (if (first state) (first (second state)) (second (second state)))))
  (fn "pair_fold_update" ((state (pair :i1 (pair :i64 :i64))) (i :i64))
      (pair :i1 (pair :i64 :i64))
    (let ((sums (pair :i64 :i64) (second state)))
      (if (< i 0)
          (pair :true (pair (- (first sums) i) (second sums)))
          (pair :false (pair (first sums) (+ (second sums) i)))))))
 (("pair_fold" (-3 3 5) 8)
  ("pair_fold" (-3 0 5) 6)
  ("pair_fold" (-3 -2 5) 3)
  ("pair_fold" (3 2 -5) -5)
  ("pair_fold" (0 3 9223372036854775807) -9223372036854775806)))
