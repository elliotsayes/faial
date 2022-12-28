__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
      if (threadIdx.x > n) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((threadIdx.x > n) && x[n] < 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
      if ((a*x[n]) > 0.0) {
          y[2 * threadIdx.x] = a*x[n];
      }
}
