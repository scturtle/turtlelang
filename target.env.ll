declare i32 @printf(i8*, ...)
@.str = private unnamed_addr constant [4 x i8] c"%d\0A\00"
declare i8* @malloc(i32)

define i32 @main() {
  %1 = call i64 @entryFunc()
  %2 = call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i64 %1)
  ret i32 0
}

@fact = global i64 0

define i64 @fact-lambda(i64 %env, i64 %n) {
entry:
  ; load the arguments, they may change value
  %env_ptr0 = alloca i64
  store i64 %env, i64* %env_ptr0
  %n_ptr = alloca i64
  store i64 %n, i64* %n_ptr
  ; load the environment
  %env_struct_ptr = inttoptr i64 %env to <{}>*
  ; the n
  %n_val = load i64* %n_ptr
  %n_zero = icmp eq i64 %n_val, 0
  br i1 %n_zero, label %if.then, label %if.else

if.then:
  br label %if.exit

if.else:
  %n_val2 = load i64* %n_ptr
  ; call
  %fact_env_ptr = load i64* @fact
  ; ptr of env and func
  %fact_env_ptr_C = inttoptr i64 %fact_env_ptr to <{ i64, i64 }>*
  %env_ptr_ptr = getelementptr inbounds <{ i64, i64 }>* %fact_env_ptr_C, i32 0, i32 0
  %env_ptr = load i64* %env_ptr_ptr
  %func_ptr_ptr = getelementptr inbounds <{ i64, i64 }>* %fact_env_ptr_C, i32 0, i32 1
  %func_ptr = load i64* %func_ptr_ptr
  %func_ptr_C = inttoptr i64 %func_ptr to i64 (i64, i64)*
  ; operands
  %n_val3 = load i64* %n_ptr
  %n_m_1 = sub i64 %n_val3, 1
  %rec = call i64 %func_ptr_C(i64 %env_ptr, i64 %n_m_1)
  %res = mul i64 %n_val2, %rec
  br label %if.exit

if.exit:
  %result = phi i64 [ 1, %if.then ], [ %res, %if.else ]
  ret i64 %result
}

define void @init(){
  %func_env_raw = call i8* @malloc(i32 16) ; two ptr
  %func_env_int = ptrtoint i8* %func_env_raw to i64
  %func_env_ptr = inttoptr i64 %func_env_int to <{ i64, i64 }>*
  %env_raw = call i8* @malloc(i32 0)
  %env_int = ptrtoint i8* %env_raw to i64
  %env_ptr = inttoptr i64 %env_int to <{}>*
  %first = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr, i32 0, i32 0
  store i64 %env_int, i64* %first
  %second = getelementptr inbounds <{ i64, i64 }>* %func_env_ptr, i32 0, i32 1
  store i64 ptrtoint (i64 (i64, i64)* @fact-lambda to i64), i64* %second
  store i64 %func_env_int, i64* @fact
  ret void
}

define i64 @entryFunc(){
  call void @init()
  %fact_env_ptr = load i64* @fact
  %fact_env_ptr_C = inttoptr i64 %fact_env_ptr to <{ i64, i64 }>*
  %env_ptr_ptr = getelementptr inbounds <{ i64, i64 }>* %fact_env_ptr_C, i32 0, i32 0
  %env_ptr = load i64* %env_ptr_ptr
  %func_ptr_ptr = getelementptr inbounds <{ i64, i64 }>* %fact_env_ptr_C, i32 0, i32 1
  %func_ptr = load i64* %func_ptr_ptr
  %func_ptr_C = inttoptr i64 %func_ptr to i64 (i64, i64)*
  %result = call i64 %func_ptr_C(i64 %env_ptr, i64 4)
  ret i64 %result
}
