// pass
// --blockDim=[32,32,1] --gridDim=2

__global__
void saxpy(int n, float a, float *x, float *y)
{
  if (threadIdx.x < 16) // half of the warp is enabled
  y[32*n + threadIdx.x] = a; // half of the cost
}
