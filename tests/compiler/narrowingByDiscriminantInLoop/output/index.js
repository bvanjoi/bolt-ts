// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingByDiscriminantInLoop.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
function insertInterface(callbackType) {
  for ( var memberType of callbackType.members) {
    if (memberType.type === 'const') {
      memberType.idlType;
    // string
    } else if (memberType.type === 'operation') {
      memberType.idlType.origin;
      // string
      (memberType.idlType);
    }
    
    
  }
}
function insertInterface2(callbackType) {
  for ( var memberType of callbackType.members) {
    if (memberType.type === 'operation') {
      memberType.idlType.origin;
    // string
    }
    
  }
}
function foo(memberType) {
  if (memberType.type === 'const') {
    memberType.idlType;
  // string
  } else if (memberType.type === 'operation') {
    memberType.idlType.origin;
  // string
  }
  
  
}// Repro for issue similar to #8383

function f1(x) {
  while (true) {
    x.prop;
    if (x.kind === true) {
      x.prop.a;
    }
    
    if (x.kind === false) {
      x.prop.b;
    }
    
  }
}
function f2(x) {
  while (true) {
    if (x.kind) {
      x.prop.a;
    }
    
    if (!x.kind) {
      x.prop.b;
    }
    
  }
}