
__device__ static __attribute__((always_inline)) void
reduceBlocks(unsigned int blockSize, int nIsPow2, const float *w, const float *x, const unsigned int n)
{
    extern __shared__ float y[];
    int i = blockIdx.x*blockDim.x + threadIdx.x;
    if (i < n) y[i] = blockSize*x[i] + y[i + 1];
}

__global__ void saxpy(unsigned int blockSize, int nIsPow2, const float *g_idata, float *g_odata, unsigned int n)
{
  reduceBlocks(blockSize, nIsPow2, g_idata, g_odata, n);
}
