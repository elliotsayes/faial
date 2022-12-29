// control-dep, drf
__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (x[i]) y[i] = n*x[i];
  y[i] = a*x[i];
}