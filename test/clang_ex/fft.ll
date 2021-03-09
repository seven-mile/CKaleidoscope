; ModuleID = 'fft.cpp'
source_filename = "fft.cpp"
target datalayout = "e-m:e-i64:64-f80:128-n8:16:32:64-S128"
target triple = "x86_64-pc-linux-gnu"

@rev = dso_local global [2000003 x i32] zeroinitializer, align 16
@cnt = dso_local global i32 0, align 4
@n = dso_local global i32 0, align 4
@m = dso_local global i32 0, align 4
@a = dso_local global [2000003 x i32] zeroinitializer, align 16
@b = dso_local global [2000003 x i32] zeroinitializer, align 16
@.str = private unnamed_addr constant [10 x i8] c"here! [1]\00", align 1
@.str.1 = private unnamed_addr constant [10 x i8] c"here! [2]\00", align 1
@.str.2 = private unnamed_addr constant [10 x i8] c"here! [3]\00", align 1
@.str.3 = private unnamed_addr constant [10 x i8] c"here! [4]\00", align 1
@.str.4 = private unnamed_addr constant [4 x i8] c"%d \00", align 1

; Function Attrs: noinline optnone uwtable
define dso_local i32 @_Z2rdv() #0 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i8, align 1
  store i32 0, i32* %1, align 4
  store i32 1, i32* %2, align 4
  br label %4

4:                                                ; preds = %16, %0
  %5 = call i32 @getchar()
  %6 = trunc i32 %5 to i8
  store i8 %6, i8* %3, align 1
  %7 = sext i8 %6 to i32
  %8 = call i32 @isdigit(i32 %7) #5
  %9 = icmp ne i32 %8, 0
  %10 = xor i1 %9, true
  br i1 %10, label %11, label %17

11:                                               ; preds = %4
  %12 = load i8, i8* %3, align 1
  %13 = sext i8 %12 to i32
  %14 = icmp eq i32 %13, 45
  br i1 %14, label %15, label %16

15:                                               ; preds = %11
  store i32 -1, i32* %2, align 4
  br label %16

16:                                               ; preds = %15, %11
  br label %4

17:                                               ; preds = %4
  br label %18

18:                                               ; preds = %30, %17
  %19 = load i8, i8* %3, align 1
  %20 = sext i8 %19 to i32
  %21 = call i32 @isdigit(i32 %20) #5
  %22 = icmp ne i32 %21, 0
  br i1 %22, label %23, label %33

23:                                               ; preds = %18
  %24 = load i32, i32* %1, align 4
  %25 = mul nsw i32 %24, 10
  %26 = load i8, i8* %3, align 1
  %27 = sext i8 %26 to i32
  %28 = add nsw i32 %25, %27
  %29 = sub nsw i32 %28, 48
  store i32 %29, i32* %1, align 4
  br label %30

30:                                               ; preds = %23
  %31 = call i32 @getchar()
  %32 = trunc i32 %31 to i8
  store i8 %32, i8* %3, align 1
  br label %18

33:                                               ; preds = %18
  %34 = load i32, i32* %1, align 4
  %35 = load i32, i32* %2, align 4
  %36 = mul nsw i32 %34, %35
  ret i32 %36
}

; Function Attrs: nounwind readonly
declare dso_local i32 @isdigit(i32) #1

declare dso_local i32 @getchar() #2

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @_Z4qpowii(i32, i32) #3 {
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  store i32 %0, i32* %3, align 4
  store i32 %1, i32* %4, align 4
  store i32 1, i32* %5, align 4
  br label %6

6:                                                ; preds = %31, %2
  %7 = load i32, i32* %4, align 4
  %8 = icmp ne i32 %7, 0
  br i1 %8, label %9, label %34

9:                                                ; preds = %6
  %10 = load i32, i32* %4, align 4
  %11 = and i32 %10, 1
  %12 = icmp ne i32 %11, 0
  br i1 %12, label %13, label %22

13:                                               ; preds = %9
  %14 = load i32, i32* %5, align 4
  %15 = sext i32 %14 to i64
  %16 = mul nsw i64 1, %15
  %17 = load i32, i32* %3, align 4
  %18 = sext i32 %17 to i64
  %19 = mul nsw i64 %16, %18
  %20 = srem i64 %19, 998244353
  %21 = trunc i64 %20 to i32
  store i32 %21, i32* %5, align 4
  br label %22

22:                                               ; preds = %13, %9
  %23 = load i32, i32* %3, align 4
  %24 = sext i32 %23 to i64
  %25 = mul nsw i64 1, %24
  %26 = load i32, i32* %3, align 4
  %27 = sext i32 %26 to i64
  %28 = mul nsw i64 %25, %27
  %29 = srem i64 %28, 998244353
  %30 = trunc i64 %29 to i32
  store i32 %30, i32* %3, align 4
  br label %31

31:                                               ; preds = %22
  %32 = load i32, i32* %4, align 4
  %33 = ashr i32 %32, 1
  store i32 %33, i32* %4, align 4
  br label %6

34:                                               ; preds = %6
  %35 = load i32, i32* %5, align 4
  ret i32 %35
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local i32 @_Z2sti(i32) #3 {
  %2 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  %3 = load i32, i32* %2, align 4
  %4 = icmp sge i32 %3, 998244353
  br i1 %4, label %5, label %8

5:                                                ; preds = %1
  %6 = load i32, i32* %2, align 4
  %7 = sub nsw i32 %6, 998244353
  store i32 %7, i32* %2, align 4
  br label %8

8:                                                ; preds = %5, %1
  %9 = load i32, i32* %2, align 4
  %10 = icmp slt i32 %9, 0
  br i1 %10, label %11, label %14

11:                                               ; preds = %8
  %12 = load i32, i32* %2, align 4
  %13 = add nsw i32 %12, 998244353
  store i32 %13, i32* %2, align 4
  br label %14

14:                                               ; preds = %11, %8
  %15 = load i32, i32* %2, align 4
  ret i32 %15
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @_Z2iti(i32) #3 {
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  store i32 %0, i32* %2, align 4
  store i32 1, i32* %3, align 4
  br label %4

4:                                                ; preds = %24, %1
  %5 = load i32, i32* %3, align 4
  %6 = load i32, i32* %2, align 4
  %7 = icmp slt i32 %5, %6
  br i1 %7, label %8, label %27

8:                                                ; preds = %4
  %9 = load i32, i32* %3, align 4
  %10 = ashr i32 %9, 1
  %11 = sext i32 %10 to i64
  %12 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %11
  %13 = load i32, i32* %12, align 4
  %14 = ashr i32 %13, 1
  %15 = load i32, i32* %2, align 4
  %16 = ashr i32 %15, 1
  %17 = load i32, i32* %3, align 4
  %18 = and i32 %17, 1
  %19 = mul nsw i32 %16, %18
  %20 = or i32 %14, %19
  %21 = load i32, i32* %3, align 4
  %22 = sext i32 %21 to i64
  %23 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %22
  store i32 %20, i32* %23, align 4
  br label %24

24:                                               ; preds = %8
  %25 = load i32, i32* %3, align 4
  %26 = add nsw i32 %25, 1
  store i32 %26, i32* %3, align 4
  br label %4

27:                                               ; preds = %4
  ret void
}

; Function Attrs: noinline nounwind optnone uwtable
define dso_local void @_Z3nttPiii(i32*, i32, i32) #3 {
  %4 = alloca i32*, align 8
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  %7 = alloca i32, align 4
  %8 = alloca i32, align 4
  %9 = alloca i32, align 4
  %10 = alloca i32, align 4
  %11 = alloca i32, align 4
  %12 = alloca i32, align 4
  %13 = alloca i32, align 4
  %14 = alloca i32, align 4
  %15 = alloca i32, align 4
  %16 = alloca i32, align 4
  %17 = alloca i32, align 4
  store i32* %0, i32** %4, align 8
  store i32 %1, i32* %5, align 4
  store i32 %2, i32* %6, align 4
  store i32 0, i32* %7, align 4
  br label %18

18:                                               ; preds = %56, %3
  %19 = load i32, i32* %7, align 4
  %20 = load i32, i32* %5, align 4
  %21 = icmp slt i32 %19, %20
  br i1 %21, label %22, label %59

22:                                               ; preds = %18
  %23 = load i32, i32* %7, align 4
  %24 = load i32, i32* %7, align 4
  %25 = sext i32 %24 to i64
  %26 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %25
  %27 = load i32, i32* %26, align 4
  %28 = icmp slt i32 %23, %27
  br i1 %28, label %29, label %55

29:                                               ; preds = %22
  %30 = load i32*, i32** %4, align 8
  %31 = load i32, i32* %7, align 4
  %32 = sext i32 %31 to i64
  %33 = getelementptr inbounds i32, i32* %30, i64 %32
  %34 = load i32, i32* %33, align 4
  store i32 %34, i32* %8, align 4
  %35 = load i32*, i32** %4, align 8
  %36 = load i32, i32* %7, align 4
  %37 = sext i32 %36 to i64
  %38 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %37
  %39 = load i32, i32* %38, align 4
  %40 = sext i32 %39 to i64
  %41 = getelementptr inbounds i32, i32* %35, i64 %40
  %42 = load i32, i32* %41, align 4
  %43 = load i32*, i32** %4, align 8
  %44 = load i32, i32* %7, align 4
  %45 = sext i32 %44 to i64
  %46 = getelementptr inbounds i32, i32* %43, i64 %45
  store i32 %42, i32* %46, align 4
  %47 = load i32, i32* %8, align 4
  %48 = load i32*, i32** %4, align 8
  %49 = load i32, i32* %7, align 4
  %50 = sext i32 %49 to i64
  %51 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @rev, i64 0, i64 %50
  %52 = load i32, i32* %51, align 4
  %53 = sext i32 %52 to i64
  %54 = getelementptr inbounds i32, i32* %48, i64 %53
  store i32 %47, i32* %54, align 4
  br label %55

55:                                               ; preds = %29, %22
  br label %56

56:                                               ; preds = %55
  %57 = load i32, i32* %7, align 4
  %58 = add nsw i32 %57, 1
  store i32 %58, i32* %7, align 4
  br label %18

59:                                               ; preds = %18
  store i32 2, i32* %9, align 4
  br label %60

60:                                               ; preds = %152, %59
  %61 = load i32, i32* %9, align 4
  %62 = load i32, i32* %5, align 4
  %63 = icmp sle i32 %61, %62
  br i1 %63, label %64, label %155

64:                                               ; preds = %60
  %65 = load i32, i32* %9, align 4
  %66 = sdiv i32 998244352, %65
  %67 = call i32 @_Z4qpowii(i32 3, i32 %66)
  store i32 %67, i32* %10, align 4
  %68 = load i32, i32* %6, align 4
  %69 = xor i32 %68, -1
  %70 = icmp ne i32 %69, 0
  br i1 %70, label %74, label %71

71:                                               ; preds = %64
  %72 = load i32, i32* %10, align 4
  %73 = call i32 @_Z4qpowii(i32 %72, i32 998244351)
  store i32 %73, i32* %10, align 4
  br label %74

74:                                               ; preds = %71, %64
  store i32 0, i32* %11, align 4
  br label %75

75:                                               ; preds = %147, %74
  %76 = load i32, i32* %11, align 4
  %77 = load i32, i32* %5, align 4
  %78 = icmp slt i32 %76, %77
  br i1 %78, label %79, label %151

79:                                               ; preds = %75
  store i32 1, i32* %12, align 4
  %80 = load i32, i32* %11, align 4
  store i32 %80, i32* %13, align 4
  br label %81

81:                                               ; preds = %143, %79
  %82 = load i32, i32* %13, align 4
  %83 = load i32, i32* %11, align 4
  %84 = load i32, i32* %9, align 4
  %85 = ashr i32 %84, 1
  %86 = add nsw i32 %83, %85
  %87 = icmp slt i32 %82, %86
  br i1 %87, label %88, label %146

88:                                               ; preds = %81
  %89 = load i32, i32* @cnt, align 4
  %90 = add nsw i32 %89, 1
  store i32 %90, i32* @cnt, align 4
  %91 = load i32, i32* @cnt, align 4
  %92 = icmp eq i32 %91, 1000
  br i1 %92, label %93, label %94

93:                                               ; preds = %88
  store i32 0, i32* @cnt, align 4
  br label %94

94:                                               ; preds = %93, %88
  %95 = load i32*, i32** %4, align 8
  %96 = load i32, i32* %13, align 4
  %97 = sext i32 %96 to i64
  %98 = getelementptr inbounds i32, i32* %95, i64 %97
  %99 = load i32, i32* %98, align 4
  store i32 %99, i32* %14, align 4
  %100 = load i32, i32* %12, align 4
  %101 = sext i32 %100 to i64
  %102 = mul nsw i64 1, %101
  %103 = load i32*, i32** %4, align 8
  %104 = load i32, i32* %13, align 4
  %105 = load i32, i32* %9, align 4
  %106 = ashr i32 %105, 1
  %107 = add nsw i32 %104, %106
  %108 = sext i32 %107 to i64
  %109 = getelementptr inbounds i32, i32* %103, i64 %108
  %110 = load i32, i32* %109, align 4
  %111 = sext i32 %110 to i64
  %112 = mul nsw i64 %102, %111
  %113 = srem i64 %112, 998244353
  %114 = trunc i64 %113 to i32
  store i32 %114, i32* %15, align 4
  %115 = load i32, i32* %14, align 4
  %116 = load i32, i32* %15, align 4
  %117 = add nsw i32 %115, %116
  %118 = call i32 @_Z2sti(i32 %117)
  %119 = load i32*, i32** %4, align 8
  %120 = load i32, i32* %13, align 4
  %121 = sext i32 %120 to i64
  %122 = getelementptr inbounds i32, i32* %119, i64 %121
  store i32 %118, i32* %122, align 4
  %123 = load i32, i32* %14, align 4
  %124 = add nsw i32 %123, 998244353
  %125 = load i32, i32* %15, align 4
  %126 = sub nsw i32 %124, %125
  %127 = call i32 @_Z2sti(i32 %126)
  %128 = load i32*, i32** %4, align 8
  %129 = load i32, i32* %13, align 4
  %130 = load i32, i32* %9, align 4
  %131 = ashr i32 %130, 1
  %132 = add nsw i32 %129, %131
  %133 = sext i32 %132 to i64
  %134 = getelementptr inbounds i32, i32* %128, i64 %133
  store i32 %127, i32* %134, align 4
  %135 = load i32, i32* %12, align 4
  %136 = sext i32 %135 to i64
  %137 = mul nsw i64 1, %136
  %138 = load i32, i32* %10, align 4
  %139 = sext i32 %138 to i64
  %140 = mul nsw i64 %137, %139
  %141 = srem i64 %140, 998244353
  %142 = trunc i64 %141 to i32
  store i32 %142, i32* %12, align 4
  br label %143

143:                                              ; preds = %94
  %144 = load i32, i32* %13, align 4
  %145 = add nsw i32 %144, 1
  store i32 %145, i32* %13, align 4
  br label %81

146:                                              ; preds = %81
  br label %147

147:                                              ; preds = %146
  %148 = load i32, i32* %9, align 4
  %149 = load i32, i32* %11, align 4
  %150 = add nsw i32 %149, %148
  store i32 %150, i32* %11, align 4
  br label %75

151:                                              ; preds = %75
  br label %152

152:                                              ; preds = %151
  %153 = load i32, i32* %9, align 4
  %154 = shl i32 %153, 1
  store i32 %154, i32* %9, align 4
  br label %60

155:                                              ; preds = %60
  %156 = load i32, i32* %6, align 4
  %157 = xor i32 %156, -1
  %158 = icmp ne i32 %157, 0
  br i1 %158, label %187, label %159

159:                                              ; preds = %155
  store i32 0, i32* %16, align 4
  %160 = load i32, i32* %5, align 4
  %161 = call i32 @_Z4qpowii(i32 %160, i32 998244351)
  store i32 %161, i32* %17, align 4
  br label %162

162:                                              ; preds = %183, %159
  %163 = load i32, i32* %16, align 4
  %164 = load i32, i32* %5, align 4
  %165 = icmp slt i32 %163, %164
  br i1 %165, label %166, label %186

166:                                              ; preds = %162
  %167 = load i32*, i32** %4, align 8
  %168 = load i32, i32* %16, align 4
  %169 = sext i32 %168 to i64
  %170 = getelementptr inbounds i32, i32* %167, i64 %169
  %171 = load i32, i32* %170, align 4
  %172 = sext i32 %171 to i64
  %173 = mul nsw i64 1, %172
  %174 = load i32, i32* %17, align 4
  %175 = sext i32 %174 to i64
  %176 = mul nsw i64 %173, %175
  %177 = srem i64 %176, 998244353
  %178 = trunc i64 %177 to i32
  %179 = load i32*, i32** %4, align 8
  %180 = load i32, i32* %16, align 4
  %181 = sext i32 %180 to i64
  %182 = getelementptr inbounds i32, i32* %179, i64 %181
  store i32 %178, i32* %182, align 4
  br label %183

183:                                              ; preds = %166
  %184 = load i32, i32* %16, align 4
  %185 = add nsw i32 %184, 1
  store i32 %185, i32* %16, align 4
  br label %162

186:                                              ; preds = %162
  br label %187

187:                                              ; preds = %186, %155
  ret void
}

; Function Attrs: noinline norecurse optnone uwtable
define dso_local i32 @main() #4 {
  %1 = alloca i32, align 4
  %2 = alloca i32, align 4
  %3 = alloca i32, align 4
  %4 = alloca i32, align 4
  %5 = alloca i32, align 4
  %6 = alloca i32, align 4
  store i32 0, i32* %1, align 4
  %7 = call i32 @_Z2rdv()
  store i32 %7, i32* @n, align 4
  %8 = call i32 @_Z2rdv()
  store i32 %8, i32* @m, align 4
  %9 = call i32 @puts(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str, i64 0, i64 0))
  store i32 0, i32* %2, align 4
  br label %10

10:                                               ; preds = %19, %0
  %11 = load i32, i32* %2, align 4
  %12 = load i32, i32* @n, align 4
  %13 = icmp sle i32 %11, %12
  br i1 %13, label %14, label %22

14:                                               ; preds = %10
  %15 = call i32 @_Z2rdv()
  %16 = load i32, i32* %2, align 4
  %17 = sext i32 %16 to i64
  %18 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @a, i64 0, i64 %17
  store i32 %15, i32* %18, align 4
  br label %19

19:                                               ; preds = %14
  %20 = load i32, i32* %2, align 4
  %21 = add nsw i32 %20, 1
  store i32 %21, i32* %2, align 4
  br label %10

22:                                               ; preds = %10
  store i32 0, i32* %3, align 4
  br label %23

23:                                               ; preds = %32, %22
  %24 = load i32, i32* %3, align 4
  %25 = load i32, i32* @m, align 4
  %26 = icmp sle i32 %24, %25
  br i1 %26, label %27, label %35

27:                                               ; preds = %23
  %28 = call i32 @_Z2rdv()
  %29 = load i32, i32* %3, align 4
  %30 = sext i32 %29 to i64
  %31 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @b, i64 0, i64 %30
  store i32 %28, i32* %31, align 4
  br label %32

32:                                               ; preds = %27
  %33 = load i32, i32* %3, align 4
  %34 = add nsw i32 %33, 1
  store i32 %34, i32* %3, align 4
  br label %23

35:                                               ; preds = %23
  store i32 1, i32* %4, align 4
  br label %36

36:                                               ; preds = %44, %35
  %37 = load i32, i32* %4, align 4
  %38 = load i32, i32* @n, align 4
  %39 = load i32, i32* @m, align 4
  %40 = add nsw i32 %38, %39
  %41 = add nsw i32 %40, 1
  %42 = icmp sle i32 %37, %41
  br i1 %42, label %43, label %47

43:                                               ; preds = %36
  br label %44

44:                                               ; preds = %43
  %45 = load i32, i32* %4, align 4
  %46 = shl i32 %45, 1
  store i32 %46, i32* %4, align 4
  br label %36

47:                                               ; preds = %36
  %48 = load i32, i32* %4, align 4
  call void @_Z2iti(i32 %48)
  %49 = load i32, i32* %4, align 4
  call void @_Z3nttPiii(i32* getelementptr inbounds ([2000003 x i32], [2000003 x i32]* @a, i64 0, i64 0), i32 %49, i32 1)
  %50 = call i32 @puts(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.1, i64 0, i64 0))
  %51 = load i32, i32* %4, align 4
  call void @_Z3nttPiii(i32* getelementptr inbounds ([2000003 x i32], [2000003 x i32]* @b, i64 0, i64 0), i32 %51, i32 1)
  store i32 0, i32* %5, align 4
  br label %52

52:                                               ; preds = %74, %47
  %53 = load i32, i32* %5, align 4
  %54 = load i32, i32* %4, align 4
  %55 = icmp slt i32 %53, %54
  br i1 %55, label %56, label %77

56:                                               ; preds = %52
  %57 = load i32, i32* %5, align 4
  %58 = sext i32 %57 to i64
  %59 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @a, i64 0, i64 %58
  %60 = load i32, i32* %59, align 4
  %61 = sext i32 %60 to i64
  %62 = mul nsw i64 1, %61
  %63 = load i32, i32* %5, align 4
  %64 = sext i32 %63 to i64
  %65 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @b, i64 0, i64 %64
  %66 = load i32, i32* %65, align 4
  %67 = sext i32 %66 to i64
  %68 = mul nsw i64 %62, %67
  %69 = srem i64 %68, 998244353
  %70 = trunc i64 %69 to i32
  %71 = load i32, i32* %5, align 4
  %72 = sext i32 %71 to i64
  %73 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @a, i64 0, i64 %72
  store i32 %70, i32* %73, align 4
  br label %74

74:                                               ; preds = %56
  %75 = load i32, i32* %5, align 4
  %76 = add nsw i32 %75, 1
  store i32 %76, i32* %5, align 4
  br label %52

77:                                               ; preds = %52
  %78 = call i32 @puts(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.2, i64 0, i64 0))
  %79 = load i32, i32* %4, align 4
  call void @_Z3nttPiii(i32* getelementptr inbounds ([2000003 x i32], [2000003 x i32]* @a, i64 0, i64 0), i32 %79, i32 -1)
  %80 = call i32 @puts(i8* getelementptr inbounds ([10 x i8], [10 x i8]* @.str.3, i64 0, i64 0))
  store i32 0, i32* %6, align 4
  br label %81

81:                                               ; preds = %93, %77
  %82 = load i32, i32* %6, align 4
  %83 = load i32, i32* @n, align 4
  %84 = load i32, i32* @m, align 4
  %85 = add nsw i32 %83, %84
  %86 = icmp sle i32 %82, %85
  br i1 %86, label %87, label %96

87:                                               ; preds = %81
  %88 = load i32, i32* %6, align 4
  %89 = sext i32 %88 to i64
  %90 = getelementptr inbounds [2000003 x i32], [2000003 x i32]* @a, i64 0, i64 %89
  %91 = load i32, i32* %90, align 4
  %92 = call i32 (i8*, ...) @printf(i8* getelementptr inbounds ([4 x i8], [4 x i8]* @.str.4, i64 0, i64 0), i32 %91)
  br label %93

93:                                               ; preds = %87
  %94 = load i32, i32* %6, align 4
  %95 = add nsw i32 %94, 1
  store i32 %95, i32* %6, align 4
  br label %81

96:                                               ; preds = %81
  ret i32 0
}

declare dso_local i32 @puts(i8*) #2

declare dso_local i32 @printf(i8*, ...) #2

attributes #0 = { noinline optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #1 = { nounwind readonly "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #2 = { "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #3 = { noinline nounwind optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #4 = { noinline norecurse optnone uwtable "correctly-rounded-divide-sqrt-fp-math"="false" "disable-tail-calls"="false" "less-precise-fpmad"="false" "min-legal-vector-width"="0" "no-frame-pointer-elim"="true" "no-frame-pointer-elim-non-leaf" "no-infs-fp-math"="false" "no-jump-tables"="false" "no-nans-fp-math"="false" "no-signed-zeros-fp-math"="false" "no-trapping-math"="false" "stack-protector-buffer-size"="8" "target-cpu"="x86-64" "target-features"="+cx8,+fxsr,+mmx,+sse,+sse2,+x87" "unsafe-fp-math"="false" "use-soft-float"="false" }
attributes #5 = { nounwind readonly }

!llvm.module.flags = !{!0}
!llvm.ident = !{!1}

!0 = !{i32 1, !"wchar_size", i32 4}
!1 = !{!"clang version 9.0.0-2~ubuntu18.04.2 (tags/RELEASE_900/final)"}
