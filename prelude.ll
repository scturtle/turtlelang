@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00"
declare i32 @printf(i8*, ...)
declare i64 @entryFunc()

define i32 @main() {
  %1 = call i64 @entryFunc()
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i64 %1)
  ret i32 0
}
