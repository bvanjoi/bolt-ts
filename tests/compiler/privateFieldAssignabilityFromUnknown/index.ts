// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privateFieldAssignabilityFromUnknown.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

export class Class {
  #field: any
  //~[target=ES5]^ ERROR: Private identifiers are only available when targeting ECMAScript 2015 and higher.
}

const task: Class = {} as unknown;
//~^ ERROR: Type 'unknown' is not assignable to type 'Class'.
