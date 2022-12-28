__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[2 * threadIdx.x * threadIdx.x] = a*x[i];
}
