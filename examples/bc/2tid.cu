// pass
// --blockDim=[1024,1,1] --gridDim=2
__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[gridDim.x * threadIdx.x] = a*x[i];
  y[2 * threadIdx.y] = a*x[i];
  y[2 * threadIdx.z] = a*x[i];
}
