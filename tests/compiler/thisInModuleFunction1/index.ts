// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInModuleFunction1.ts`, Apache-2.0 License

module bar {
  export function bar() {
   return this;
   //~^ ERROR: 'this' implicitly has type 'any' because it does not have a type annotation.
  } 
} 
var z = bar.bar();
