// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualTypingOfAccessors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

// not contextually typing accessors

var x: {
   foo: (x: number) => number;
}
 
x = {
   get foo() {
      return (n)=>n
   },
   set foo(x) {}
}

const a: number = x.foo(1);