// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/contextualTypingOfAccessors.ts`, Apache-2.0 License

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