declare i32 @printf(i8*, ...)
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00"
declare i8* @malloc(i32)
declare void @free(i8*)

define i32 @main() {
  %1 = call i64 @entryFunc()
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i64 %1)
  ret i32 0
}

define i64 @fact(<{}>* %env, i64 %n) {
entry:
  ; load the arguments
  %n_ptr = alloca i64
  store i64 %n, i64* %n_ptr
  ; the n
  %n_val = load i64* %n_ptr
  %n_zero = icmp eq i64 %n_val, 0
  br i1 %n_zero, label %if.then, label %if.else

if.then:
  br label %if.exit

if.else:
  %n_val2 = load i64* %n_ptr
  ; gen new env for call
  %env_raw = call i8* @malloc(i32 0)
  %env_ptr = bitcast i8* %env_raw to <{}>*
  ; gen arg for call
  %n_val3 = load i64* %n_ptr
  %n_m_1 = sub i64 %n_val3, 1
  ; call
  %rec = call i64 @fact(<{}>* %env_ptr, i64 %n_m_1)
  ; free the env
  call void @free(i8* %env_raw)
  %res = mul i64 %n_val2, %rec
  br label %if.exit

if.exit:
  %result = phi i64 [ 1, %if.then ], [ %res, %if.else ]
  ret i64 %result
}

define i64 @entryFunc(){
  ; gen new env for call
  %env_raw = call i8* @malloc(i32 0)
  %env_ptr = bitcast i8* %env_raw to <{}>*
  ; gen arg for call
  ; call
  %result = call i64 @fact(<{}>* %env_ptr, i64 4)
  ; free the env
  call void @free(i8* %env_raw)
  ret i64 %result
}
