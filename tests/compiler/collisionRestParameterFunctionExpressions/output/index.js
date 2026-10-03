function foo() {
  function f1(_i, ...restParameters) {
    var _i = 10;
  }
  function f1NoError(_i) {
    var _i = 10;
  }
  function f3(...restParameters) {
    var _i = 10;
  }
  function f3NoError() {
    var _i = 10;
  }
  function f4(_i, ...rest) {}
  function f4NoError(_i) {}
}