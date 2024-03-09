__shared__ float y[1024][1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (i < n) y[i][i] = a*x[i] + y[i + 1][i + 1];
}
