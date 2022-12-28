__shared__ double y[1024];
__shared__ double A[32][32+1];
__shared__ short g[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  // RaCUDA does not support shorts
  // g[16 * threadIdx.x] -> 7 bank-conflicts because g is a short (hence 1/2 of bank-conflicts)
  // y[threadIdx.x] -> 1 bank-conflict because y is a double (2 * word)
  y[threadIdx.x] = (double) a*x[i] + g[16 * threadIdx.x];
}
