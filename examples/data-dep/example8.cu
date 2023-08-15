// ind, drf
__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  if (x[i]) {
    int w = i + 1;
    x[w] = 0;
  }
}
