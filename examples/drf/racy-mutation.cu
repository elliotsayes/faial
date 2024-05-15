__global__
void saxpy(float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int flag = 0;
  if (i % 2) {
    flag = 1;
  }
  y[i] = a*x[i];
  if (flag) {
    y[i + 1] = a*x[i];
  }
}
