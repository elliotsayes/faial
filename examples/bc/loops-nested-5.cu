__shared__ float y[1024];

__global__
void saxpy(int n, int m, int o, int p, int q, float a, float *x)
{
  for (int i = 0; i < q; i++) {
    for (int j = i; j < p; j++) {
      for (int k = j; k < o; k++) {
        for (int w = k; w < n; w++) {
        for (int z = w; z < m; z++) {
        y[2 * threadIdx.x] = a*x[i];
        }
        }
      }
    }
  }

}
