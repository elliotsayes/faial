// pass
// --gridDim=[2,2,2]
__global__
void saxpy(int n, float a, float *x, float *y, int k)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) {
    y[i + 2] = a*x[i] +
      y[i + gridDim.x] +
      y[i + gridDim.y] +
      y[i + gridDim.z];
  }
}
