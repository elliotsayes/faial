#define N 256
__device__ float accessX(float a, float y[][N]) {
  return a * y[blockIdx.x][threadIdx.x];
}

__global__
void saxpy(int n, float a, float x[][N], float *y)
{
  __assume(blockDim.y == 1 && blockDim.z == 1);
  __assume(gridDim.y == 1 && gridDim.z == 1);
  __assume(N == blockDim.x);
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = accessX(a, x) + y[i];
}
