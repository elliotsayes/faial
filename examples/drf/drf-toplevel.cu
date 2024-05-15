// Data-race free as long as analysis understand top-level assignments
const unsigned int A = 0;

__global__
void saxpy(int n, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = x[i] + y[i + A];
}
