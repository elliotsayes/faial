__device__ unsigned int get(int* array, int index) {
  return array[index];
}

__global__
void saxpy(int* array, int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  // a bug where the variable being returned from a kernel call
  // is being ignored which makes `offset` available as a
  // global variable
  int offset = get(array, i);
  y[i + offset] = a*x[i] + y[i + offset];
}
