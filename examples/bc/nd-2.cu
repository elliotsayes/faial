__shared__ float y[32][32];
__shared__ float z[32][32+1];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[threadIdx.x][threadIdx.y] = a*x[i];
  y[threadIdx.y][threadIdx.x] = a*x[i];
  z[threadIdx.x][threadIdx.y] = a*x[i];
}
