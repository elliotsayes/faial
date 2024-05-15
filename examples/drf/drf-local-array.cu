//pass
//--gridDim=[32,32,1]      --blockDim=[16,16,1]

typedef unsigned int  uint;

__device__ int spurious(float *array1)
{
    float f1;
    float f2;
    *array1 = f1;
    return f1 > f2;
}

extern __device__ uint func();

__global__ void
d_render(uint *global, uint w)
{

    uint x = blockIdx.x*blockDim.x + threadIdx.x;
    uint y = blockIdx.y*blockDim.y + threadIdx.y;

    if (x >= w) return;

    // find intersection with box
    float local;
    int hit = spurious(&local);

    // write output color
    global[y * w + x] = func();
}
