// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/contextualTyping13.ts`, Apache-2.0 License

var foo:(a:number)=>number = function(a){return a};

var bar: (a: number) => number = function (b) {
  let a: string = b;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  return b
}

var bar2: (a: number) => number = function (b) {
  let a: string = a;
  //~^ ERROR: Block-scoped variable 'a' used before its declaration.
  return b
}
