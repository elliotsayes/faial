__global__
void saxpy(unsigned int n, float a, float *x, float *y)
{
  float f = 0.0f;
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[0] = 0; // benign data-race
  for (int j = n; j > 0; j--) {
    f += y[j];
  }
  y[i + n + 1] = a*x[i] + f;
}
