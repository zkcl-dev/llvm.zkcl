define i64 @positive_part(i64 %x) {
entry:
  %positive = icmp sgt i64 %x, 0
  br i1 %positive, label %keep, label %floor

keep:
  br label %join

floor:
  br label %join

join:
  %answer = phi i64 [ %x, %keep ], [ 0, %floor ]
  ret i64 %answer
}
