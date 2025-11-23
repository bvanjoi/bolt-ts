// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/noTypeArgumentOnReturnType1.ts`, Apache-2.0 License

class A<T>{
 
 foo(): A{
  //~^ ERROR: Generic type 'A<T>' requires 1 type argument.
  return null;
 }
}
