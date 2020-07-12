/*
  Since the LLVM KaleidoscopeJIT would find symbols in the
  executable file, so we can implement standard library
  for CKaleidoscope with cpp and record it in a header
  file.
  The dll is needed to run jit.
*/

#include <bits/stdc++.h>

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
  return printf("%f\n", val);
}
