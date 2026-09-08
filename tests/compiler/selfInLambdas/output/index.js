
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