// pass
// --blockDim=32
__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  __shared__ uint data;
  if (i < n) {
    data = i; // <- should be a data-race
    y[i] = a*x[i] + y[i];
  }
}
