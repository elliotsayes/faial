__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  for (int i = 0; i < n; i++) {
    for (int j = i; j < n; j++) {
      for (int k = j; k < n; k++) {
        for (int w = k; w < n; w++) {
        y[2 * threadIdx.x] = a*x[i];
        }
      }
    }
  }

}
