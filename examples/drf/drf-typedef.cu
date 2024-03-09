typedef int foo;
__global__
void saxpy(int n, float a, float *x, foo *y)
{
  foo i = blockIdx.x*blockDim.x + threadIdx.x;
  foo j = y[i];
  assert (i < n);
  y[i] = a*x[j] + y[i];
}
