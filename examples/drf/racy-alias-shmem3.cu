__global__

void saxpy(int n, float a, float *x, float *y)
{
  extern __shared__ float sram[];
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float *z = &sram[i * 3];
  if (i < n) sram[i * 3] = a*x[i] + z[3];
}
