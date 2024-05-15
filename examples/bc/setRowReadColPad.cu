//pass
//--blockDim=32 --gridDim=32
#define BDIMY 32
#define BDIMX 32
#define IPAD 1
__global__ void setRowReadColPad(int *out) {
    __shared__ int tile[BDIMY][BDIMX + IPAD];
    unsigned int tidy_tidx = threadIdx.y * blockDim.x + threadIdx.x;
    tile[threadIdx.y][threadIdx.x] = tidy_tidx;
    __syncthreads();
    out[tidy_tidx] = tile[threadIdx.x][threadIdx.y];
}
