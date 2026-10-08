// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularObjectLiteralAccessors.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
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