//saxpy,ind,ctrl
//_unknown_1, _unknown_2, k
/*
Example 4: data flows from array to upper bound of loop

Approximate variables are `x[i]`, then `y[i]` and `k` (because
it depends on `x[i]`.

 */
__global__ void saxpy(int n, float a, float *x, float *y) {
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i]; // upper bound comes from array x
  for (int k = 1; k < j; k++) {
    y[i + 1] = a * y[i];
  }
}
