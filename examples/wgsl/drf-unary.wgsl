override x : u32;

@group(0) @binding(0) var<storage, read_write> data: array<f32>;

@compute @workgroup_size(256, 1, 1) fn computeSomething(
  @builtin(workgroup_id) blockIdx : vec3<u32>,
  @builtin(num_workgroups) gridDim : vec3<u32>,
  @builtin(local_invocation_id) threadIdx : vec3<u32>
) {
  if (! (x == 0)) { return ; }
  let i = blockIdx.x*256 + threadIdx.x;
  data[i + x] = data[i] * 2.0;
}
