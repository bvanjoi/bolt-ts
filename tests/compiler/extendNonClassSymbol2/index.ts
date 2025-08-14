// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/extendNonClassSymbol2.ts`, Apache-2.0 License

function Foo() {
   this.x = 1;
}
var x = new Foo(); // legal, considered a constructor function
class C extends Foo {} // error, could not find symbol Foo
//~^ ERROR:  Type '() => void' is not a constructor function type.