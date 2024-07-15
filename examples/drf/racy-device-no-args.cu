__device__ void helper(float data) {
  extern __shared__ float y[];
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i] = data + y[i + 1];
}
__global__
void saxpy(float a, float *x)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  helper(a*x[i]);
}
