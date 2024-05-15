//pass
// --blockDim=32
// This example is only DRF in the presence of synchronous warps, otherwise
// this would be racy.
__global__
void saxpy_racy(int n, float a, float *shmem, float *y)
{
  unsigned tid = threadIdx.x;
  shmem[tid] += shmem[tid+16];
  shmem[tid] += shmem[tid+8];
  shmem[tid] += shmem[tid+4];
  shmem[tid] += shmem[tid+2];
  shmem[tid] += shmem[tid+1];
}

