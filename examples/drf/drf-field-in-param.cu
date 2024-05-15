struct foo {
    int x;
    int z;
    int* data;
};

__global__ void hello(float* output, foo f) {
  output[threadIdx.x + f.x] = 2 * output[threadIdx.x + f.x];
}
