var ObjectLiteral = {};
(function (ObjectLiteral) {

  var ThisInObjectLiteral = {
      _foo: '1',
    get foo() {
      return this._foo;
    },
    set foo(value) {
      this._foo = value;
    },
    test: function () {
      return this._foo;
    }    
  };
  
})(ObjectLiteral);