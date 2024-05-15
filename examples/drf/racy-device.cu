__device__ float *y;

__global__ void saxpy(float a, float *x) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i] = a*x[i] + y[i + 1];
}
