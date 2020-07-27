/*
  Since the LLVM KaleidoscopeJIT would find symbols in the
  executable file, so we can implement standard library
  for CKaleidoscope with cpp and record it in a header
  file.
  The dll is needed to run jit.
*/

#include <bits/stdc++.h>
#include <cstdio>

#define STD_API extern "C"

// extern number putch(char c);
STD_API double putch(char c) {
  return putchar(c);
}

// extern number print(string str);
STD_API double print(char *str) {
  return printf("%s", str);
}

// extern number printd(number val);
STD_API double printd(double val) {
  return printf("%f", val);
}

// extern number scand(...);
// through Kaleidoscope currently doesn't
// support pointer type, you can use va_arg ((foggy
STD_API double scand(double* num) {
  return scanf("%lf", num);
}

// extern number freopen_in(string file);
STD_API double freopen_in(const char * file) {
  freopen(file, "r", stdin);
  return 0;
}

// extern number freopen_out(string file);
STD_API double freopen_out(const char * file) {
  freopen(file, "w", stdout);
  return 0;
}
