// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticsInAFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

function boo{
  //~^ ERROR: Expected '('.
   static test()
   //~^ ERROR: Declaration or statement expected.
   //~| ERROR: Cannot find name 'test'.
   static test(name:string)
   //~^ ERROR: Declaration or statement expected.
   //~| ERROR: Expected ','.
   //~| ERROR: Argument expression expected.
   //~| ERROR: Cannot find name 'test'.
   //~| ERROR: Cannot find name 'string'.
   //~| ERROR: Cannot find name 'name'.
   static test(name?:any){}
   //~^ ERROR: Declaration or statement expected.
   //~| ERROR: Expression expected.
   //~| ERROR: Cannot find name 'test'.
   //~| ERROR: Cannot find name 'any'.
   //~| ERROR: Cannot find name 'name'.
   //~| ERROR: Unexpected keyword or identifier.
}
