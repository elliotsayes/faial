//pass
//--blockDim=2024 --gridDim=2024
// This example is only racy at the grid level.
__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = threadIdx.x;
  __syncthreads();
  if (i < n) y[i] = a*x[i] + y[i];
}
