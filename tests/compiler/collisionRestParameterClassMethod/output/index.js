class c1 {
  foo(_i, ...restParameters) {
    var _i = 10;
  }
  fooNoError(_i) {
    var _i = 10;
  }
  f4(_i, ...rest) {
    var _i;
  }
  f4NoError(_i) {
    var _i;
  }
}
class c3 {
  foo(...restParameters) {
    var _i = 10;
  }
  fooNoError() {
    var _i = 10;
  }
}