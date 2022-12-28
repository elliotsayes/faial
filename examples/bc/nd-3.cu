__shared__ float y[32][10][8];
__shared__ float z[33][32][31];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[threadIdx.x][threadIdx.y][threadIdx.z] = a*x[i];
  y[threadIdx.y][threadIdx.x][threadIdx.z] = a*x[i];
  z[threadIdx.y][threadIdx.x][threadIdx.z] = a*x[i];
}
