// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/controlFlowWithIncompleteTypes.ts`, Apache-2.0 License

function foo1() {
  var x = 0;
  while (cond) {
    if (typeof x === 'string') {
      x = x.slice();
    } else {
      x = 'abc';
    }
    
  }
}
function foo2() {
  var x = 0;
  while (cond) {
    if (typeof x === 'number') {
      x = 'abc';
    } else {
      x = x.slice();
    }
    
  }
}