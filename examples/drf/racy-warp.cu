__global__
void saxpy_racy(int n, float a, float *shmem, float *y)
{
  unsigned tid = threadIdx.x;
  // Incorrect use of __syncwarp()
  shmem[tid] += shmem[tid+16]; __syncwarp();
  shmem[tid] += shmem[tid+8]; __syncwarp();
  shmem[tid] += shmem[tid+4]; __syncwarp();
  shmem[tid] += shmem[tid+2]; __syncwarp();
  shmem[tid] += shmem[tid+1]; __syncwarp();
}

