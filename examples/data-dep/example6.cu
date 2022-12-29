// control-dep, drf
__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  for (int k = j; k < n; k++) {
    y[i] = y[i];
  }
}