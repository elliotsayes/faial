__device__ unsigned int get(int);

__global__
void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  // a bug was making the return of get(i) generate
  // the same variable __unkn0 in both assignments,
  // which meant that offset1 == offset2
  int offset1 = get(i);
  int offset2 = get(i);
  assert(__uniform_int(offset1));
  assert(__uniform_int(offset2));
  y[i + offset1] = a*x[i] + y[i + offset2];
}
