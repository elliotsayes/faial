@group(0) @binding(0) var<storage, read_write> data: array<f32>;

@compute @workgroup_size(256, 1, 1) fn computeSomething(
  @builtin(workgroup_id) blockIdx : vec3<u32>,
  @builtin(num_workgroups) gridDim : vec3<u32>,
  @builtin(local_invocation_id) threadIdx : vec3<u32>
) {
  if (gridDim.y != 1 || gridDim.z != 1) { return ; }
  let j = blockIdx.x*256 + threadIdx.x;
  var a: i32 = 2;
  var i: i32 = 0;
  loop {
    if i % 2 == 0 { continue; }
    a = a * 2;
    data[j] = data[j] * 2.0;
    continuing {
      i = i + 1;
      break if i >= 4;
    }
  }
}
