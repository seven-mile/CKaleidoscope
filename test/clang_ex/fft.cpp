#include <cctype>
#include <cstdio>

int rd() {
  int x=0,f=1;char c;while(!isdigit(c=getchar())){
    if(c=='-')f=-1;
  }
  for(;isdigit(c);c=getchar()){
    x=x*10+c-'0';
  }
  return x*f;
}

const int N = 2000003, M = 998244353, G = 3;

int qpow(int a, int b) {
  int r = 1;
  for (; b; b = b >> 1) {
    if (b & 1) r = 1ll * r * a % M;
    a = 1ll * a * a % M;
  }
  return r;
}

int st(int x) {
  if (x>=M) x-=M; if (x<0) x+=M; return x;
}

int rev[N];

void it(int n) {
  for (int i=1; i<n; i++)
    rev[i] = (rev[i>>1]>>1)|((n>>1)*(i&1));
}

int cnt = 0;

void ntt(int *a, int n, int d) {
  for (int i=0; i<n; i++) if(i<rev[i]) {
    int tmp = a[i];
    a[i] = a[rev[i]];
    a[rev[i]] = tmp;
  }
  for (int h=2; h<=n; h<<=1) {
    int wn = qpow(G, (M-1)/h);
    if (!~d) wn = qpow(wn, M-2);
    for (int j=0; j<n; j+=h) {
      int w = 1;
      for (int k=j; k<j+(h>>1); k++) {
        cnt++;
        if (cnt == 1000) {
          cnt = 0;
        }
        int x = a[k], y = 1ll*w*a[k+(h>>1)]%M;
        a[k] = st(x + y); a[k+(h>>1)] = st(x + M - y);
        w=1ll*w*wn%M;
      }
    }
  }
  if (!~d) for (int i=0, j=qpow(n,M-2); i<n; i++) a[i]=1ll*a[i]*j%M;
}

int n, m, a[N], b[N];

int main() {
  n = rd();
  m = rd();
  puts("here! [1]");
  for (int i=0; i<=n; i++) a[i] = rd();
  for (int i=0; i<=m; i++) b[i] = rd();
  int lim = 1;
  for (; lim <= n+m+1; lim<<=1);
  it(lim);
  ntt(a, lim, 1);
  puts("here! [2]");
  ntt(b, lim, 1);

  for (int i=0; i<lim; i++) a[i] = 1ll*a[i]*b[i]%M;
  puts("here! [3]");
  ntt(a, lim, -1);
  puts("here! [4]");
  
  for (int i=0; i<=n+m; i++) printf("%d ", a[i]);

  return 0;
}
