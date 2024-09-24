@group(0) @binding(0) var<storage, read_write> data: array<i32>;
@group(0) @binding(1) var<storage, read_write> out: array<i32>;

@compute @workgroup_size(256, 1, 1) fn computeSomething(
  @builtin(workgroup_id) blockIdx : vec3<u32>,
  @builtin(num_workgroups) gridDim : vec3<u32>,
  @builtin(local_invocation_id) threadIdx : vec3<u32>
) {
  if (gridDim.y != 1 || gridDim.z != 1) { return ; }
  let i = blockIdx.x*256 + threadIdx.x;
  var x = data[i];
  if (x == i) {
    out[i] = out[x] * 2;
  }
}
