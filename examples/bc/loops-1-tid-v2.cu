__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
  int k =  threadIdx.x;
  for (int i = 0; i < k; i++) {
      y[2 * threadIdx.x] = a*x[i];
  }
}
