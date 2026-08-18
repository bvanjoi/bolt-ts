// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/anyAndUnknownHaveFalsyComponents.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

var y1 = x1 && 3;

function foo1() {
  return {
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
    })    
  };
}