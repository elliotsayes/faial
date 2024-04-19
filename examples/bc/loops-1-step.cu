__shared__ float y[1024];

__global__
void saxpy(int lb, int ub, float a, float *x)
{
  for (int j = ub; j >= lb; j -= 5) {
    y[2 * threadIdx.x] = a*x[j];
  }
}
