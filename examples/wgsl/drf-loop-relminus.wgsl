var<storage, read> n: i32;
var<storage, read> a: f32;
var<storage, read_write> y: array<f32>;
const blockDim : vec3<u32> = vec3(256,1,1);
@compute @workgroup_size(blockDim.x, blockDim.y, blockDim.z) fn saxpy (
  @builtin(workgroup_id) blockIdx : vec3<u32>,
  @builtin(num_workgroups) gridDim : vec3<u32>,
  @builtin(local_invocation_id) threadIdx : vec3<u32>
)
{
  if (n <= 0) { return; }
  let i = blockIdx.x*blockDim.x + threadIdx.x;
  var f = 0.0f;
  y[0] = 0;
  for (var j = 1; j - (n - 1); j++) {
    f += y[j];
    // y[0] ... y[n - 1]
  }
  // y[ 0 + n]
  y[i + n] = f;
}
