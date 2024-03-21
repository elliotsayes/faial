enum GridSamplerInterpolationMode { BILINEAR = 0, NEAREST, BICUBIC };

__global__
void saxpy(float a, float *x, float *y, GridSamplerInterpolationMode mode)
{
  int i = threadIdx.x;
  if (i < 4) {
    y[i + 4] = a*x[i] + y[i + mode];
  }
}
