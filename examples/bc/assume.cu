// pass
// --blockDim=[1024,1,1] --gridDim=2
__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  __assume(n == 2);
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[n * threadIdx.x] = a*x[i];
}
