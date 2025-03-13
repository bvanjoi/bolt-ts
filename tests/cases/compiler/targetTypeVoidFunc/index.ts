// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/targetTypeVoidFunc.ts`, Apache-2.0 License

function f1(): { new (): number; } {
  return function () { return; }
  //~^ ERROR: Type '() => void' is not assignable to type 'new () => number'.
}; 

var x = f1();
var y = new x();
var z = new (f1())();