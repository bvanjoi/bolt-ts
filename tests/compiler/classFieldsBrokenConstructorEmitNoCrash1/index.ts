// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/classFieldsBrokenConstructorEmitNoCrash1.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: useDefineForClassFields=false
//@compiler-options: target=es2021

class Test {
  prop = 42;
  constructor
  //~^ ERROR: Constructor implementation is missing. 
} //~ ERROR: Expected '('.