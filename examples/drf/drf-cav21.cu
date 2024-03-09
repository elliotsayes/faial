__global__
void saxpy(int N, float a, float *x, float *y)
{
  for (int r = 0; r < N; r++) { 
    for (int i = 0; i < N; i++) {
      y[threadIdx.x] = a*x[threadIdx.x];
    }
    __syncthreads();
    for (int j = 0; j < N; j++) {
      x[threadIdx.x] = y[threadIdx.x + j];
    }
    __syncthreads();
  }
}
