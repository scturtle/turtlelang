@.str = private unnamed_addr constant [3 x i8] c"%d\00"

define i32 @fact(i32 %x) {
  %1 = icmp eq i32 %x, 0
  br i1 %1, label %then, label %else

then:
  br label %end

else:
  %2 = sub i32 %x, 1
  %3 = call i32 @fact(i32 %2)
  %4 = mul i32 %3, %x
  br label %end

end:
  %5 = phi i32 [1, %then], [%4, %else]
  ret i32 %5
}

define i32 @main() {
  %1 = call i32 @fact(i32 5)
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([3 x i8]* @.str, i32 0, i32 0), i32 %1)
  ret i32 0
}

declare i32 @printf(i8*, ...)
