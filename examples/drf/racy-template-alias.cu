template<typename T>
__global__ void saxpy(T a, float *x, T *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  auto w = y + i;
  *w = a*x[i] + w[1];
}
