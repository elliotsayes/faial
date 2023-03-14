//pass
//--blockDim=[32,32,1] --gridDim=[16,1,1]

__global__ void kernel(int *__dummy, int *va, int *vb, int *vresult)
{
    __shared__ int localA[256];
    int __dummylocalA;
   // changing i += 16 to i ++ fixes the issue
    for (int i = 0; i < 256; i += 16) {
        for (int k = 0; k < 16; k += 1) {
           // changing threadIdx.y to threadIdx.x fixes the issue
           // using threadIdx.z also triggers the issue
            __dummylocalA = localA[threadIdx.y];
        }
    }
}
