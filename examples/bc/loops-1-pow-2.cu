__shared__ float y[1024];

// A multiplicative loop where the bound is known and a literal
__global__
void saxpy(int n, float a, float *x)
{
  for (int i = 1; i < 1024; i *= 2) {
    y[i * threadIdx.x] = a*x[i];
  }
}
