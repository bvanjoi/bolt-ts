// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/thisInModuleFunction1.ts`, Apache-2.0 License

module bar {
  export function bar() {
   return this;
  } 
} 
var z = bar.bar();