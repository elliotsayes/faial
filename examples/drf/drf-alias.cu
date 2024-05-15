__global__

void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float *z = &y[i];
  if (i < n) y[i] = a*x[i] + z[0];
}
