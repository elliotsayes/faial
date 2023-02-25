// pass
// --blockDim=[1024,1,1] --gridDim=2
__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[2 * threadIdx.x] = a*x[i];
}

__global__
void saxpy2(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[2 * threadIdx.x] = a*x[i];
}
