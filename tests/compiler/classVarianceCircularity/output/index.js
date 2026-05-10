function f() {
  var b = new Bar();
  console.log(b.Value);
}
class Bar {
  num;
  Field = (this).num;
  Value = (this).num;
}