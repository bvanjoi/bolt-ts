// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowDestructuringLoop.ts`, Apache-2.0 License
function isNumVal(x) {
  return typeof x.val === 'number';
}
function foo(things) {
  for ( var thing of things) {
    if (isNumVal(thing)) {
      var {val} = thing;
      val.toFixed(2);
    } else {
      var {val} = thing;
      val.length;
    }
    
  }
}