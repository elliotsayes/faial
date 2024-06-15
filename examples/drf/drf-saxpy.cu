__global__
void saxpy(int n, float a, float *x, float *y)
{
  __assume(blockDim.y == 1 && blockDim.z == 1);
  __assume(gridDim.y == 1 && gridDim.z == 1);
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = a*x[i] + y[i];
}
