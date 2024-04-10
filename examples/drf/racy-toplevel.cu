// Racy, but we should check if we understand top-level assignments
// check if it's racy with: --tid1 0 --tid2 1
const unsigned int A = 1;

__global__
void saxpy(int n, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i] = x[i] + y[i + A];
}
