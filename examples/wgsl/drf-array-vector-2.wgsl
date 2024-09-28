@group(0) @binding(0) var<storage, read_write> data: array<vec3<f32>>;

@compute @workgroup_size(64) fn computeSomething(
  @builtin(global_invocation_id) threadIdx: vec3<u32>
) {
  data[threadIdx.x].x = data[threadIdx.x + 1].y * 2.0;
}
