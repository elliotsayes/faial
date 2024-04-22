// pass
// --blockDim=[1,32,1]
__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  // Racuda: 1
  // faial-bc: 1
  y[2 * threadIdx.y] = a*x[i];
}
