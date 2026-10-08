// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/discriminantNarrowingCouldBeCircular.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: lib=[es2022]
//@run-fail
// #57705, 57690
var o = {};
if (o) {
  for ( var key in o) {
    var value = o[key];
    if (is(value)) {}
    
  }
}


function getImplicitAriaRole(element) {
  var ancestor = element;
  while (ancestor) {
    var parent = parentElementOrShadowHost(ancestor);
    var parents = kPresentationInheritanceParents[ancestor.a];
    if (!parents || !parent || !parents.includes(parent.a)) break;
    
    ancestor = parent;
  }
}

if (isPlainObject2(myObj2)) {
  for ( var key of ['a', 'b', 'c']) {
    var deeper = myObj2[key];
    var deeperKeys = isPlainObject2(deeper) ? Object.keys(deeper) : [];
  }
}
