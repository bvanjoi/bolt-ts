// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/selfInLambdas.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: lib=[es5]
//@compiler-options: noImplicitAny
//@compiler-options: noImplicitThis

var o = {
  counter: 0,
  start: function () {
    window.onmousemove = () => {
      this.counter++;
      var f = () => (this.counter);
    };
  }  
};
class X {
  value = 'value';
  foo() {
    var outer = () => {
      var x = this.value;
      var inner = () => {
        var y = this.value;
      };
      inner();
    };
    outer();
  }
}