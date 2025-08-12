class Bug {
  name
  static func = [(that, name) => {
    that.foo(name);
  }]
  foo(name) {
    this.name = name;
  }
}
class A {
  prop1 = function () {
    this;
  }
  prop2 = function () {
    function inner() {
      this;
    }
    () => (this);
  }
  prop3 = () => {
    function inner() {
      this;
    }
  }
  prop4 = {
      a: function () {
      return this
    }    
  }
  prop5 = () => ({
      a: function () {
      return this
    }    
  })
}
class B {
  prop1 = this
  prop2 = () => (this)
  prop3 = () => (() => (() => (() => (this))))
  prop4 = '  ' + function () {} + ' ' + (() => (() => (() => (this))))
  prop5 = {
      a: () => (this)    
  }
  prop6 = () => ({
      a: () => (this)    
  })
}