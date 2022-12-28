__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[2 * threadIdx.x + n] = a*x[i];
  y[n - 2 * threadIdx.x] = a*x[i]; // racuda really doesn't like this
  y[2 * threadIdx.x + (n * 20)] = a*x[i];
  y[n * threadIdx.x + (n * 20)] = a*x[i];
}
