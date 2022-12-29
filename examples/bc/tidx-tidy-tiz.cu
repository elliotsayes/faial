__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[32 * threadIdx.x] = a*x[i];
  y[32 * threadIdx.y] = a*x[i];
  y[32 * threadIdx.z] = a*x[i];
//  y[threadIdx.x * threadIdx.y * threadIdx.z + n] = a*x[i];
}
