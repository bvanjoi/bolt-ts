// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/doNotElaborateAssignabilityToTypeParameters.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

async function foo<T>(x: T): Promise<T> {
  let yaddable = await getXOrYadda(x);
  return yaddable;
  //~^ ERROR: Type 'Yadda | Awaited' is not assignable to type 'T'.
}

interface Yadda {
  stuff: string,
  things: string,
}

declare function getXOrYadda<T>(x: T): T | Yadda;
