// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/voidUndefinedReduction.ts`, Apache-2.0 License

//@compiler-options: strict

function isDefined<T>(value: T | undefined | null | void): value is T {
  return value !== undefined && value !== null;
}

declare const foo: string | undefined;

if (isDefined(foo)) {
  console.log(foo.toUpperCase()); 
} 

if (isDefined(foo)) {
} else {
  foo.toUpperCase()
  //~^ ERROR: 'foo' is possibly undefined.
}

if (!isDefined(foo)) {
  foo.toUpperCase()
  //~^ ERROR: 'foo' is possibly undefined.
}