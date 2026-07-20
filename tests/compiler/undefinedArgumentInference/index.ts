// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/undefinedArgumentInference.ts`, Apache-2.0 License

function foo1<T>(f1: { x: T; y: T }): T {

  return undefined;
  //~^ ERROR: Type 'undefined' is not assignable to type 'T'.

}

var z1 = foo1({ x: undefined, y: undefined }); 
