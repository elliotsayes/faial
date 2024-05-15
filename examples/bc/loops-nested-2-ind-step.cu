__shared__ float y[1024];

__global__
void saxpy(int n, int m, float a, float *x, int step1, int step2) {
  for (int i = 0; i < n; i += step1) {
    for (int j = i; j < m; j += step2) {
      y[2 * threadIdx.x] = a*x[i];
    }
  }
}
