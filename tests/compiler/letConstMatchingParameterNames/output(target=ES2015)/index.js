var parent = true;
var parent2 = true;
function a() {
  var parent = 1;
  var parent2 = 2;
  function b(parent, parent2) {
    use(parent);
    use(parent2);
  }
}