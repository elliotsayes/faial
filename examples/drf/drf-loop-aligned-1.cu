__global__
void saxpy(int n, float a, float *x, float *y)
{
  __requires (n > 0);
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  float f = 0.0f;
  y[i] = 0.0;
  __syncthreads();
  for (int j = 1; j < n; j++) {
    f += y[j];
    __syncthreads();
  }
  y[i + 1] = 0.0;
}
