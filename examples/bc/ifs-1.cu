__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  if (threadIdx.x <= 16) {
    y[10 + 2 * threadIdx.x] = a; // cost: 1
  } else {
    y[4 * threadIdx.x] = a; // cost: 1
  }
}
