@group(0) @binding(0) var<storage, read_write> data: array<array<f32, 3>>;

@compute @workgroup_size(64) fn computeSomething(
  @builtin(global_invocation_id) id: vec3<u32>
) {
  let col = id.x;
  let row = id.y;

  data[row][col] = data[row][col] * 2.0;
}
