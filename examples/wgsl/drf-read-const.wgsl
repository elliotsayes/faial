// specify a global of floats array called `data`
@group(0) @binding(0) var<storage, read_write> source: array<f32>;
@group(0) @binding(0) var<storage, read_write> data: array<f32>;

@compute @workgroup_size(1) fn computeSomething(
  @builtin(global_invocation_id) id: vec3<u32>
) {
  let i = id.x;
  data[i] = source[0] * 2.0;
}
