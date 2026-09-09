// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/expressionsForbiddenInParameterInitializers.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: strict=false
//@compiler-options: lib=[es6]

export async function foo({ foo = await import("./bar") }) {
  //~^ ERROR: 'await' expressions cannot be used in a parameter initializer.
  //~| ERROR: Cannot invoke an object which is possibly 'undefined'.
}

export function* foo2({ foo = yield "a" }) {
  //~^ ERROR: 'yield' expressions cannot be used in a parameter initializer.
}

export function* foo3({ foo = { a: yield "a" } }) {
  //~^ ERROR: 'yield' expressions cannot be used in a parameter initializer.
}