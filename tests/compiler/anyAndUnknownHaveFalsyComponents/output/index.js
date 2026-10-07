
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anyAndUnknownHaveFalsyComponents.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@run-fail
var y1 = x1 && 3;

function foo1() {
  return // #39113
  {
      display: 'block',
    ...(isTreeHeader1 && {
          display: 'flex'      
    })    
  };
}

var y2 = x2 && 3;

function foo2() {
  return {
      display: 'block',
    ...(isTreeHeader1 && {
          display: 'flex'      
    // #39113
    })    
  };
}