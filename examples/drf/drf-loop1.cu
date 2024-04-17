__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float f = 0.0f;
  for (int j = 0; j < n; j++) {
    f += y[j];
    // y[0] ... y[n - 1]
  }
  // y[ 0 + n]
  y[i + n] = f;
}
