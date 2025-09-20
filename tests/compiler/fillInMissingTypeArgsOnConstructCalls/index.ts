// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/fillInMissingTypeArgsOnConstructCalls.ts`, Apache-2.0 License

class A<T extends Object>{
      list: T ;
}
var a = new A();