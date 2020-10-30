#include <bits/stdc++.h>
#include <cstdio>

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