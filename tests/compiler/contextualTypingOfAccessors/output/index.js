var x;
x = {
  get foo() {
    return (n) => (n)
  },
  set foo(x) {}  
};
var a = x.foo(1);