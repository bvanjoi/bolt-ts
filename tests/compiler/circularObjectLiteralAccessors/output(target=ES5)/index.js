var a = {
  b: {
      get foo() {
      return a.foo;
    },
    set foo(value) {
      a.foo = value;
    }    
  },
  foo: ''  
};