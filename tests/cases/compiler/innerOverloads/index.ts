// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/innerOverloads.ts`, Apache-2.0 License

function outer() {
  function inner(x:number); // should work
  function inner(x:string);
  function inner(a:any) { return a; }

  return inner(0);
}

var x = outer(); // should work

