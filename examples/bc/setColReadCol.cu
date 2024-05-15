//pass
//--blockDim=32 --gridDim=32
#define BDIMY 32
#define BDIMX 32
__global__ void setRowReadRow(int *out) {
    __shared__ int tile[BDIMY][BDIMX];
    unsigned int tidy_tidx = threadIdx.y * blockDim.x + threadIdx.x;
    tile[threadIdx.x][threadIdx.y] = tidy_tidx;
    __syncthreads();
    out[tidy_tidx] = tile[threadIdx.x][threadIdx.y];
}
