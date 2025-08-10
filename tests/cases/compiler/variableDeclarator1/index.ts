// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/variableDeclarator1.ts`, Apache-2.0 License

var a = function () {
  var c = 1;
  return c;
};

function f() {
  b0 += 1;
  b1 += b2;
}

b0 += 1;
b1 += b2;
//~^ ERROR: Block-scoped variable 'b1' used before its declaration.
//~| ERROR: Block-scoped variable 'b2' used before its declaration.
var b0 = 42;
let b1 = 42;
const b2 = 42;
