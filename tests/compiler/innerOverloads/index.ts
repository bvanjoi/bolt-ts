// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/innerOverloads.ts`, Apache-2.0 License

function outer() {
  function inner(x:number); // should work
  //~^ ERROR: 'inner', which lacks return-type annotation, implicitly has an 'any' return type.
  function inner(x:string);
  //~^ ERROR: 'inner', which lacks return-type annotation, implicitly has an 'any' return type.
  function inner(a:any) { return a; }

  return inner(0);
}

var x = outer(); // should work

