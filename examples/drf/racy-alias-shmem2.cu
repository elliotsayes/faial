__global__

void saxpy(int n, float a, float *x, float *y)
{
  extern __shared__ float sram[];
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float *z = sram;
  if (i < n) sram[i] = a*x[i] + z[i + 1];
}
