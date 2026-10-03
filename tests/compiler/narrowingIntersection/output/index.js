function test1(result) {
  if (result.err) {
    throw result.err
  }
  
  return result.value;
}
function want0(x) {}
function test2(a) {
  if (a === 0) {
    want0(a);
  }
  
}