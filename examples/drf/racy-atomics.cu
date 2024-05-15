__global__
void saxpy(int n, float a, float *x, int *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i] = (int) a*x[i] + y[i];
  atomicInc(&y[i + 1], 1);
}
