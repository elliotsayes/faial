// pass
// --blockDim=[1024,1,1] --gridDim=2
__shared__ float y[1024];

__device__ int f();

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[f() * threadIdx.x] = a*x[i];
}
