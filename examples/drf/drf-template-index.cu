template <typename IdxType>
__global__
void saxpy(float a, float *x, float *y, IdxType N)
{
  IdxType i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i] = a*x[i] + y[i];
}
