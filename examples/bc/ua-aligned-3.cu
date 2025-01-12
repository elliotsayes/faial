// pass
// --blockDim=[32,32,1] --gridDim=2

__global__
void saxpy(int wA, float a, float *x, float *y)
{
  y[threadIdx.y * wA + threadIdx.x] = a;
}
