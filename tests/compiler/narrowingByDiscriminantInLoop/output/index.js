function insertInterface(callbackType) {
  for ( var memberType of callbackType.members) {
    if (memberType.type === 'const') {
      memberType.idlType;
    } else if (memberType.type === 'operation') {
      memberType.idlType.origin;
      (memberType.idlType);
    }
    
    
  }
}
function insertInterface2(callbackType) {
  for ( var memberType of callbackType.members) {
    if (memberType.type === 'operation') {
      memberType.idlType.origin;
    }
    
  }
}
function foo(memberType) {
  if (memberType.type === 'const') {
    memberType.idlType;
  } else if (memberType.type === 'operation') {
    memberType.idlType.origin;
  }
  
  
}
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