__global__
void saxpy(float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j;
  y[i] = a*x[i] + y[(j = i + 1, i)];
}
