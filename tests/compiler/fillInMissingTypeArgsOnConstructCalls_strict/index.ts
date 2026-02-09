// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/fillInMissingTypeArgsOnConstructCalls.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

class A<T extends Object>{
      list: T ;
      //~^ ERROR: Property 'list' has no initializer and is not definitely assigned in the constructor.
}
var a = new A();