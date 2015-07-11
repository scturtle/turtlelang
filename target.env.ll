declare i32 @printf(i8*, ...)
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00"
declare i8* @malloc(i32)
declare void @free(i8*)

define i32 @main() {
  %1 = call i64 @entryFunc()
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i64 %1)
  ret i32 0
}

define i64 @fact(i64 %env, i64 %n) {
entry:
  ; load the arguments
  %n_ptr = alloca i64
  store i64 %n, i64* %n_ptr
  ; load the env
  %env_ptr0 = inttoptr i64 %env to <{}>*
  ; the n
  %n_val = load i64* %n_ptr
  %n_zero = icmp eq i64 %n_val, 0
  br i1 %n_zero, label %if.then, label %if.else

if.then:
  br label %if.exit

if.else:
  %n_val2 = load i64* %n_ptr
  ; gen
  %func_env_raw = call i8* @malloc(i32 16)
  %func_env_ptr = bitcast i8* %func_env_raw to <{i64, i64}>*
  %first = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr, i32 0, i32 0
  %env_raw = call i8* @malloc(i32 0)
  %env_ptr =  bitcast i8* %env_raw to <{}>*
  %env_int =  ptrtoint <{}>* %env_ptr to i64
  store i64 %env_int, i64* %first
  %second = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr, i32 0, i32 1
  store i64 ptrtoint (i64 (i64, i64)* @fact to i64), i64* %second
  %func_env_int = ptrtoint <{i64, i64}>* %func_env_ptr to i64
  ; call
  %func_env_ptr2 = inttoptr i64 %func_env_int to <{i64, i64}>*
  %first2 = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr2, i32 0, i32 0
  %env_int2 = load i64* %first2
  %second2 = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr2, i32 0, i32 1
  %func_int = load i64* %second2
  %func_ptr = inttoptr i64 %func_int to i64 (i64, i64)*
  ; arg
  %n_val3 = load i64* %n_ptr
  %n_m_1 = sub i64 %n_val3, 1
  %rec = call i64 %func_ptr(i64 %env_int2, i64 %n_m_1)
  ; call void @free(i8* %func_env_raw)
  %res = mul i64 %n_val2, %rec
  br label %if.exit

if.exit:
  %result = phi i64 [ 1, %if.then ], [ %res, %if.else ]
  ret i64 %result
}

define i64 @entryFunc(){
  ; gen
  %func_env_raw = call i8* @malloc(i32 16)
  %func_env_ptr = bitcast i8* %func_env_raw to <{i64, i64}>*
  %first = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr, i32 0, i32 0
  %env_raw = call i8* @malloc(i32 0)
  %env_ptr =  bitcast i8* %env_raw to <{}>*
  %env_int =  ptrtoint <{}>* %env_ptr to i64
  store i64 %env_int, i64* %first
  %second = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr, i32 0, i32 1
  store i64 ptrtoint (i64 (i64, i64)* @fact to i64), i64* %second
  %func_env_int = ptrtoint <{i64, i64}>* %func_env_ptr to i64
  ; call
  %func_env_ptr2 = inttoptr i64 %func_env_int to <{i64, i64}>*
  %first2 = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr2, i32 0, i32 0
  %env_int2 = load i64* %first2
  %second2 = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr2, i32 0, i32 1
  %func_int = load i64* %second2
  %func_ptr = inttoptr i64 %func_int to i64 (i64, i64)*
  %result = call i64 %func_ptr(i64 %env_int2, i64 4)
  ; call void @free(i8* %func_env_raw)
  ret i64 %result
}
