// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/thisInStaticMethod1.ts`, Apache-2.0 License

class foo {
  static x = 3;
  static bar() {
   return this.x;
  } 
} 
var x = foo.bar();
var y: string = x;
//~^ ERROR: Type 'number' is not assignable to type 'string'.