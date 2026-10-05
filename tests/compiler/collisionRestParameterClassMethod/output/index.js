class c1 {
  foo(_i, ...restParameters) {
    var _i = 10;
  }
  fooNoError(_i) {
    var _i = 10;
  }
  // no codegen no error
  f4(_i, ...rest) {
    var _i;
  }
  // no error
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