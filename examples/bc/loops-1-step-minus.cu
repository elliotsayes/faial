__shared__ float y[1024];

__global__
void saxpy(int lb, int ub, int step, float a, float *x)
{
  for (int j = ub; j >= lb; j -= step) {
    y[2 * threadIdx.x] = a*x[j];
  }
}
