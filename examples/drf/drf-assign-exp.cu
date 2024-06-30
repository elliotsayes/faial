__global__
void saxpy(float a, float *x, float *y)
{
  int i, j;
  i = j = blockIdx.x*blockDim.x;
  i = j + threadIdx.x;
  j += threadIdx.x;
  y[i] = a*x[i] + y[j];
}
