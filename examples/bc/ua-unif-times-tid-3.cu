// pass
// --blockDim=[32,32,1] --gridDim=2

__global__
void saxpy(int n, float a, float *x, float *y)
{
  if (threadIdx.x % 2)
    y[n*threadIdx.x] = a;
}
