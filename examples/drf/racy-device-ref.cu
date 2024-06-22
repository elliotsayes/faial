__device__ void f(float &x, float y) {
  x = y;
}
__global__ void saxpy(float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  f(y[i], a*x[i] + y[i + 1]);
}
