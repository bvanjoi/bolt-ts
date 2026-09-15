// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionAssignabilityWithArrayLike01.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@[strict=true]  compiler-options: strict=true
//@[strict=false] compiler-options: strict=false

function func() {}
const array: ArrayLike<any> = func;
//~^ ERROR: Type '() => void' is not assignable to type 'ArrayLike<any>'.