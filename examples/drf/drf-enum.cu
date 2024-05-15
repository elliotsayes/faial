enum GridSamplerInterpolationMode { BILINEAR = 0, NEAREST, BICUBIC };

__global__
void saxpy(float a, float *x, float *y)
{
  int i = blockIdx.x*blockDim.x + threadIdx.x;
  y[i] = a*x[i] + y[i + BILINEAR];
}
