// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingOfAccessors.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var x;
x = {
  get foo() {
    return (n) => (n);
  },
  set foo(x) {}  
};
var a = x.foo(1);