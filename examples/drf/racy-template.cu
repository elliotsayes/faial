template<typename T>
__global__ void saxpy(T a, float *x, T *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i] = a*x[i] + y[i + 1];
}
