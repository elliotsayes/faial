__shared__ float y[1024];

__global__
void saxpy(int n, int m, float a, float *x)
{
  /*
  for (int i = 0; i < n; i += 5) {
    y[2 * threadIdx.x] = a*x[i];
  }*/
  for (int j = 0; j < n; j += m) {
    y[2 * threadIdx.x] = a*x[j];
  }
}
