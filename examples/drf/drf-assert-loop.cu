//pass
//--gridDim=16 --blockDim=16
__global__ void saxpy(float* Qi, int Bc, int N, int d) {
  int tx = threadIdx.x * blockDim.x + blockIdx.x;
  for (int y = 0; y < Bc; y++) {
      if (y >= 1)
        break;
      float sum = 0;
        Qi[tx] = Qi[tx + y];
  }
}
