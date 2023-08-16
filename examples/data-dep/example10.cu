// saxpy,data,ctrl
// array data flows to loop bounds and index an array
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i];
  int k = x[i + 1];
  float sum = 0.0f;
  for (int l = j; l < k; l++) {
    sum += x[l];
    y[l] = sum;
  }
}
