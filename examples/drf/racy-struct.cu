__global__
void saxpy(float a, float *x, float2 *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i].x = a*x[i] + y[i + 1].x;
}
