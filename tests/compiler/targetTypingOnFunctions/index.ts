// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/targetTypingOnFunctions.ts`, Apache-2.0 License

function f1(): { new (): number; } {
  return function () { return; }
  //~^ ERROR: Type '() => undefined' is not assignable to type 'new () => number'.
}; 

var x = f1();
var y = new x();
var z = new (f1())();