function fn(x = () => this, y = x()) {
  return y
}
fn.call(4);