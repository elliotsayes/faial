// saxpy,data,ind
// integer flows from array index
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  if (i < n) y[j] = a*j;
}
