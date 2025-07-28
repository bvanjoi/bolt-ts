class Test {
  'prop1'
  foo() {
    var x = () => this['prop1'];
    var y = x();
  }
}