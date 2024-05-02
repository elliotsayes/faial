// pass
// --blockDim=[1024,1,1] --gridDim=2
__shared__ float y[1024];

__device__ int f(int);

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int idx1 = x[i];
  int idx2 = f(threadIdx.x);
  y[idx1] = a;
  y[idx2] = a;
  y[n] = a;
}
