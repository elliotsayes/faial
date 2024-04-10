//saxpy,ind,ctrl
//_unknown_1, j, k
/*
Example 3: data flows from array to lower bound of loop

Variable `x[i]` is `j`, `y[i + 1]` is `_unknown_1`.

*/
__global__ void saxpy(int n, float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  int j = x[i]; // lower bound comes from array x
  for (int k = j; k < n; k++) {
    y[i] = y[i + 1];
  }
}
