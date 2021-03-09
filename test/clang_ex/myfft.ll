; ModuleID = 'kaleido'
source_filename = "kaleido"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"

@N = internal constant i32 2000003
@M = internal constant i32 998244353
@G = internal constant i32 3
@rev = internal global [2000003 x i32] zeroinitializer
@cnt = internal global i32 0
@0 = private unnamed_addr constant [24 x i8] c"h = %d, j = %d, k = %d\0A\00", align 1
@n = internal global i32 0
@m = internal global i32 0
@a = internal global [2000003 x i32] zeroinitializer
@b = internal global [2000003 x i32] zeroinitializer
@1 = private unnamed_addr constant [4 x i8] c"%d \00", align 1

declare i32 @printf(i8*, ...)

declare i32 @scanf(i8*, ...)

declare i8 @getchar()

declare i8 @putchar()

declare i32 @isdigit(i8)

declare i32 @puts(i8*)

declare i32 @freopen_in(i8*)

declare i32 @freopen_out(i8*)

define i32 @rd() {
entry:
  br label %loop

loop:                                             ; preds = %afterif, %entry
  %f13 = phi i32 [ %f1317, %afterif ], [ 1, %entry ]
  %call = call i8 @getchar()
  %call1 = call i32 @isdigit(i8 %call)
  %0 = icmp eq i32 %call1, 0
  br i1 %0, label %body, label %loop3

body:                                             ; preds = %loop
  %eq = icmp eq i8 %call, 45
  br i1 %eq, label %then, label %afterif

then:                                             ; preds = %body
  br label %afterif

afterif:                                          ; preds = %body, %then
  %f1317 = phi i32 [ -1, %then ], [ %f13, %body ]
  br label %loop

loop3:                                            ; preds = %loop, %body4
  %x12 = phi i32 [ %sub, %body4 ], [ 0, %loop ]
  %c10 = phi i8 [ %call11, %body4 ], [ %call, %loop ]
  %call8 = call i32 @isdigit(i8 %c10)
  %loopcond = icmp eq i32 %call8, 0
  br i1 %loopcond, label %after6, label %body4

body4:                                            ; preds = %loop3
  %mul = mul i32 %x12, 10
  %1 = sext i8 %c10 to i32
  %add = add i32 %mul, -48
  %sub = add i32 %add, %1
  %call11 = call i8 @getchar()
  br label %loop3

after6:                                           ; preds = %loop3
  %mul14 = mul i32 %f13, %x12
  ret i32 %mul14
}

define i32 @qpow(i32 %a, i32 %b) {
entry:
  br label %loop

loop:                                             ; preds = %afterif, %entry
  %a8 = phi i32 [ %3, %afterif ], [ %a, %entry ]
  %r14 = phi i32 [ %r516, %afterif ], [ 1, %entry ]
  %b13 = phi i32 [ %shr, %afterif ], [ %b, %entry ]
  %loopcond = icmp eq i32 %b13, 0
  br i1 %loopcond, label %after, label %body

body:                                             ; preds = %loop
  %and = and i32 %b13, 1
  %ifcond = icmp eq i32 %and, 0
  br i1 %ifcond, label %else, label %then

after:                                            ; preds = %loop
  ret i32 %r14

then:                                             ; preds = %body
  %0 = sext i32 %r14 to i64
  %1 = sext i32 %a8 to i64
  %mul7 = mul nsw i64 %1, %0
  %mod = srem i64 %mul7, 998244353
  %2 = trunc i64 %mod to i32
  br label %afterif

else:                                             ; preds = %body
  %.pre = sext i32 %a8 to i64
  br label %afterif

afterif:                                          ; preds = %else, %then
  %.pre-phi = phi i64 [ %.pre, %else ], [ %1, %then ]
  %r516 = phi i32 [ %r14, %else ], [ %2, %then ]
  %mul11 = mul nsw i64 %.pre-phi, %.pre-phi
  %mod12 = srem i64 %mul11, 998244353
  %3 = trunc i64 %mod12 to i32
  %shr = lshr i32 %b13, 1
  br label %loop
}

define i32 @st(i32 %x) {
entry:
  %ge = icmp sgt i32 %x, 998244352
  %sub = add i32 %x, -998244353
  %x6 = select i1 %ge, i32 %sub, i32 %x
  %lt = icmp slt i32 %x6, 0
  %add = add i32 %x6, 998244353
  %x9 = select i1 %lt, i32 %add, i32 %x6
  %0 = xor i1 %ge, true
  %1 = xor i1 %0, true
  %2 = or i1 %1, %lt
  br i1 %2, label %3, label %4

3:                                                ; preds = %entry
  br label %4

4:                                                ; preds = %entry, %3
  ret i32 %x9
}

define void @it(i32 %n) {
entry:
  br label %loop

loop:                                             ; preds = %body, %entry
  %i10 = phi i32 [ %5, %body ], [ 1, %entry ]
  %lt = icmp slt i32 %i10, %n
  br i1 %lt, label %body, label %after

body:                                             ; preds = %loop
  %shr = lshr i32 %i10, 1
  %0 = zext i32 %shr to i64
  %1 = getelementptr [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %0
  %2 = load i32, i32* %1, align 4
  %shr5 = lshr i32 %2, 1
  %shr7 = lshr i32 %n, 1
  %and = and i32 %i10, 1
  %mul = mul nuw i32 %and, %shr7
  %or = or i32 %mul, %shr5
  %3 = sext i32 %i10 to i64
  %4 = getelementptr [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %3
  store i32 %or, i32* %4, align 4
  %5 = add i32 %i10, 1
  br label %loop

after:                                            ; preds = %loop
  ret void
}

define void @ntt(i32* %a, i32 %n, i32 %d) {
entry:
  br label %loop

loop:                                             ; preds = %afterif, %entry
  %a12 = phi i32* [ %a57127, %afterif ], [ %a, %entry ]
  %i18 = phi i32 [ %11, %afterif ], [ 0, %entry ]
  %lt = icmp slt i32 %i18, %n
  br i1 %lt, label %body, label %after

body:                                             ; preds = %loop
  %0 = sext i32 %i18 to i64
  %1 = getelementptr [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %0
  %2 = load i32, i32* %1, align 4
  %lt8 = icmp slt i32 %i18, %2
  br i1 %lt8, label %then, label %afterif

after:                                            ; preds = %loop
  %h = alloca i32, align 4
  br label %loop19

then:                                             ; preds = %body
  %3 = getelementptr i32, i32* %a12, i64 %0
  %4 = load i32, i32* %3, align 4
  %tmp = alloca i32, align 4
  store i32 %4, i32* %tmp, align 4
  %5 = sext i32 %2 to i64
  %6 = getelementptr i32, i32* %a12, i64 %5
  %7 = load i32, i32* %6, align 4
  store i32 %7, i32* %3, align 4
  %8 = load i32, i32* %1, align 4
  %9 = sext i32 %8 to i64
  %10 = getelementptr i32, i32* %a, i64 %9
  store i32 %4, i32* %10, align 4
  br label %afterif

afterif:                                          ; preds = %body, %then
  %a57127 = phi i32* [ %a, %then ], [ %a12, %body ]
  %11 = add i32 %i18, 1
  br label %loop

loop19:                                           ; preds = %after35, %after
  %a57124 = phi i32* [ %a57125, %after35 ], [ %a12, %after ]
  %h76 = phi i32 [ %shl, %after35 ], [ 2, %after ]
  store i32 %h76, i32* %h, align 4
  %le = icmp sgt i32 %h76, %n
  br i1 %le, label %after22, label %body20

body20:                                           ; preds = %loop19
  %div = sdiv i32 998244352, %h76
  %call = call i32 @qpow(i32 3, i32 %div)
  %wn = alloca i32, align 4
  store i32 %call, i32* %wn, align 4
  %12 = icmp eq i32 %d, -1
  br i1 %12, label %then27, label %afterif31

after22:                                          ; preds = %loop19
  %13 = icmp eq i32 %d, -1
  br i1 %13, label %then91, label %afterif113

then27:                                           ; preds = %body20
  %call29 = call i32 @qpow(i32 %call, i32 998244351)
  store i32 %call29, i32* %wn, align 4
  br label %afterif31

afterif31:                                        ; preds = %body20, %then27
  %wn82 = phi i32 [ %call29, %then27 ], [ %call, %body20 ]
  %j = alloca i32, align 4
  br label %loop32

loop32:                                           ; preds = %after43, %afterif31
  %a57125 = phi i32* [ %a70, %after43 ], [ %a57124, %afterif31 ]
  %h87122 = phi i32 [ %h60, %after43 ], [ %h76, %afterif31 ]
  %j51 = phi i32 [ %add88, %after43 ], [ 0, %afterif31 ]
  store i32 %j51, i32* %j, align 4
  %lt38 = icmp slt i32 %j51, %n
  br i1 %lt38, label %body33, label %after35

body33:                                           ; preds = %loop32
  %w = alloca i32, align 4
  store i32 1, i32* %w, align 4
  %k = alloca i32, align 4
  br label %loop40

after35:                                          ; preds = %loop32
  %shl = shl i32 %h87122, 1
  br label %loop19

loop40:                                           ; preds = %afterif55, %body33
  %w80 = phi i32 [ %27, %afterif55 ], [ 1, %body33 ]
  %a70 = phi i32* [ %a12, %afterif55 ], [ %a57125, %body33 ]
  %h60 = phi i32 [ %h76, %afterif55 ], [ %h87122, %body33 ]
  %j86 = phi i32 [ %j45119, %afterif55 ], [ %j51, %body33 ]
  %k85 = phi i32 [ %28, %afterif55 ], [ %j51, %body33 ]
  store i32 %k85, i32* %k, align 4
  %shr = lshr i32 %h60, 1
  %add = add i32 %shr, %j86
  %lt47 = icmp slt i32 %k85, %add
  br i1 %lt47, label %body41, label %after43

body41:                                           ; preds = %loop40
  %cnt = load i32, i32* @cnt, align 4
  %14 = add i32 %cnt, 1
  store i32 %14, i32* @cnt, align 4
  %eq = icmp eq i32 %14, 1000
  br i1 %eq, label %then49, label %afterif55

after43:                                          ; preds = %loop40
  %add88 = add i32 %h60, %j86
  br label %loop32

then49:                                           ; preds = %body41
  %call53 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([24 x i8], [24 x i8]* @0, i64 0, i64 0), i32 %h60, i32 %j51, i32 %k85)
  store i32 0, i32* @cnt, align 4
  br label %afterif55

afterif55:                                        ; preds = %body41, %then49
  %j45119 = phi i32 [ %j51, %then49 ], [ %j86, %body41 ]
  %15 = sext i32 %k85 to i64
  %16 = getelementptr i32, i32* %a70, i64 %15
  %17 = load i32, i32* %16, align 4
  %x = alloca i32, align 4
  store i32 %17, i32* %x, align 4
  %18 = sext i32 %w80 to i64
  %shr61 = lshr i32 %h60, 1
  %add62 = add i32 %shr61, %k85
  %19 = sext i32 %add62 to i64
  %20 = getelementptr i32, i32* %a70, i64 %19
  %21 = load i32, i32* %20, align 4
  %22 = sext i32 %21 to i64
  %mul64 = mul nsw i64 %22, %18
  %mod = srem i64 %mul64, 998244353
  %23 = trunc i64 %mod to i32
  %y = alloca i32, align 4
  store i32 %23, i32* %y, align 4
  %add67 = add i32 %23, %17
  %call68 = call i32 @st(i32 %add67)
  store i32 %call68, i32* %16, align 4
  %add72 = add i32 %17, 998244353
  %y73.neg = sub i32 0, %23
  %sub = add i32 %add72, %y73.neg
  %call74 = call i32 @st(i32 %sub)
  %shr77 = lshr i32 %h76, 1
  %add78 = add i32 %shr77, %k85
  %24 = sext i32 %add78 to i64
  %25 = getelementptr i32, i32* %a12, i64 %24
  store i32 %call74, i32* %25, align 4
  %26 = sext i32 %wn82 to i64
  %mul83 = mul nsw i64 %26, %18
  %mod84 = srem i64 %mul83, 998244353
  %27 = trunc i64 %mod84 to i32
  store i32 %27, i32* %w, align 4
  %28 = add i32 %k85, 1
  br label %loop40

then91:                                           ; preds = %after22
  %i92 = alloca i32, align 4
  store i32 0, i32* %i92, align 4
  %call94 = call i32 @qpow(i32 %n, i32 998244351)
  %j95 = alloca i32, align 4
  store i32 %call94, i32* %j95, align 4
  br label %loop96

loop96:                                           ; preds = %body97, %then91
  %i111 = phi i32 [ %35, %body97 ], [ 0, %then91 ]
  %lt102 = icmp slt i32 %i111, %n
  br i1 %lt102, label %body97, label %afterif113

body97:                                           ; preds = %loop96
  %29 = sext i32 %i111 to i64
  %30 = getelementptr i32, i32* %a12, i64 %29
  %31 = load i32, i32* %30, align 4
  %32 = sext i32 %31 to i64
  %33 = sext i32 %call94 to i64
  %mul107 = mul nsw i64 %33, %32
  %mod108 = srem i64 %mul107, 998244353
  %34 = trunc i64 %mod108 to i32
  store i32 %34, i32* %30, align 4
  %35 = add i32 %i111, 1
  store i32 %35, i32* %i92, align 4
  br label %loop96

afterif113:                                       ; preds = %after22, %loop96
  ret void
}

define i32 @main() {
entry:
  %call = call i32 @rd()
  store i32 %call, i32* @n, align 4
  %call1 = call i32 @rd()
  store i32 %call1, i32* @m, align 4
  br label %loop

loop:                                             ; preds = %body, %entry
  %i5 = phi i32 [ %2, %body ], [ 0, %entry ]
  %n = load i32, i32* @n, align 4
  %le = icmp sgt i32 %i5, %n
  br i1 %le, label %after, label %body

body:                                             ; preds = %loop
  %call3 = call i32 @rd()
  %0 = sext i32 %i5 to i64
  %1 = getelementptr [2000003 x i32], [2000003 x i32]* @a, i64 0, i64 %0
  store i32 %call3, i32* %1, align 4
  %2 = add i32 %i5, 1
  br label %loop

after:                                            ; preds = %loop
  %i6 = alloca i32, align 4
  br label %loop7

loop7:                                            ; preds = %body8, %after
  %i15 = phi i32 [ %5, %body8 ], [ 0, %after ]
  store i32 %i15, i32* %i6, align 4
  %m = load i32, i32* @m, align 4
  %le12 = icmp sgt i32 %i15, %m
  br i1 %le12, label %after10, label %body8

body8:                                            ; preds = %loop7
  %call13 = call i32 @rd()
  %3 = sext i32 %i15 to i64
  %4 = getelementptr [2000003 x i32], [2000003 x i32]* @b, i64 0, i64 %3
  store i32 %call13, i32* %4, align 4
  %5 = add i32 %i15, 1
  br label %loop7

after10:                                          ; preds = %loop7
  %lim = alloca i32, align 4
  %n21.pre = load i32, i32* @n, align 4
  br label %loop16

loop16:                                           ; preds = %body17, %after10
  %lim41 = phi i32 [ %shl, %body17 ], [ 1, %after10 ]
  store i32 %lim41, i32* %lim, align 4
  %add = add i32 %n21.pre, 1
  %add23 = add i32 %add, %m
  %le24 = icmp sgt i32 %lim41, %add23
  br i1 %le24, label %after19, label %body17

body17:                                           ; preds = %loop16
  %shl = shl i32 %lim41, 1
  br label %loop16

after19:                                          ; preds = %loop16
  call void @it(i32 %lim41)
  call void @ntt(i32* getelementptr inbounds ([2000003 x i32], [2000003 x i32]* @a, i64 0, i64 0), i32 %lim41, i32 1)
  call void @ntt(i32* getelementptr inbounds ([2000003 x i32], [2000003 x i32]* @b, i64 0, i64 0), i32 %lim41, i32 1)
  %i29 = alloca i32, align 4
  br label %loop30

loop30:                                           ; preds = %body31, %after19
  %i40 = phi i32 [ %14, %body31 ], [ 0, %after19 ]
  store i32 %i40, i32* %i29, align 4
  %lt = icmp slt i32 %i40, %lim41
  br i1 %lt, label %body31, label %after33

body31:                                           ; preds = %loop30
  %6 = sext i32 %i40 to i64
  %7 = getelementptr [2000003 x i32], [2000003 x i32]* @a, i64 0, i64 %6
  %8 = load i32, i32* %7, align 4
  %9 = sext i32 %8 to i64
  %10 = getelementptr [2000003 x i32], [2000003 x i32]* @b, i64 0, i64 %6
  %11 = load i32, i32* %10, align 4
  %12 = sext i32 %11 to i64
  %mul38 = mul nsw i64 %12, %9
  %mod = srem i64 %mul38, 998244353
  %13 = trunc i64 %mod to i32
  store i32 %13, i32* %7, align 4
  %14 = add i32 %i40, 1
  br label %loop30

after33:                                          ; preds = %loop30
  call void @ntt(i32* getelementptr inbounds ([2000003 x i32], [2000003 x i32]* @a, i64 0, i64 0), i32 %lim41, i32 -1)
  %i42 = alloca i32, align 4
  br label %loop43

loop43:                                           ; preds = %body44, %after33
  %i54 = phi i32 [ %18, %body44 ], [ 0, %after33 ]
  store i32 %i54, i32* %i42, align 4
  %n48 = load i32, i32* @n, align 4
  %m49 = load i32, i32* @m, align 4
  %add50 = add i32 %m49, %n48
  %le51 = icmp sgt i32 %i54, %add50
  br i1 %le51, label %after46, label %body44

body44:                                           ; preds = %loop43
  %15 = sext i32 %i54 to i64
  %16 = getelementptr [2000003 x i32], [2000003 x i32]* @a, i64 0, i64 %15
  %17 = load i32, i32* %16, align 4
  %call53 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @1, i64 0, i64 0), i32 %17)
  %18 = add i32 %i54, 1
  br label %loop43

after46:                                          ; preds = %loop43
  ret i32 0
}