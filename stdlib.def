#include <bits/stdc++.h>
#include <cstdarg>
#include <cstdio>
#include <stdexcept>

#define STD_API extern "C"

STD_API double i2d(int x) {
  return x;
}

STD_API int d2i(double x) {
  return floor(x);
}

// extern int freopen_in(string file);
STD_API int freopen_in(const char * file) {
  freopen(file, "r", stdin);
  return 0;
}

// extern int freopen_out(string file);
STD_API int freopen_out(const char * file) {
  freopen(file, "w", stdout);
  return 0;
}

STD_API int eprintf(char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  int ret = vfprintf(stderr, fmt, ap);
  va_end(ap);
  return ret;
}

