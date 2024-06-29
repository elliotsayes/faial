__global__
void saxpy(float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float f;
  y[i] = (f = y[i + 1], f + a*x[i]);
}
