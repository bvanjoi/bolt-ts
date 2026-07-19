// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/extendNonClassSymbol2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function Foo() {
   this.x = 1;
}
var x = new Foo(); // legal, considered a constructor function
class C extends Foo {} // error, could not find symbol Foo
//~^ ERROR:  Type '() => void' is not a constructor function type.
