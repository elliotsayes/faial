__shared__ float y[1024];

__global__
void saxpy(int n, float a, float *x)
{
      for (int i = 0; i < n; i++) {
          y[2 * threadIdx.x] = a*x[i];
      }
      for (int j = 0; j < n; j++) {
          y[2 * threadIdx.x] = a*x[j];
      }
      for (int k = 0; k < n; k++) {
          y[2 * threadIdx.x] = a*x[k];
      }
     for (int k1 = 0; k1 < n; k1++) {
          y[2 * threadIdx.x] = a*x[k1];
      }
     for (int k2 = 0; k2 < n; k2++) {
          y[2 * threadIdx.x] = a*x[k2];
      }
     for (int k3 = 0; k3 < n; k3++) {
          y[2 * threadIdx.x] = a*x[k3];
      }

      x[threadIdx.x] = y[threadIdx.x];
}
