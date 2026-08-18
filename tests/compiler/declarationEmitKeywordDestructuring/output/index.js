// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitKeywordDestructuring.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f1({enum: _enum, ...rest}) {
  return rest;
}
function f2({function: _function, ...rest}) {
  return rest;
}
function f3({abstract: _abstract, ...rest}) {
  return rest;
}
function f4({async: _async, ...rest}) {
  return rest;
}
function f5({await: _await, ...rest}) {
  return rest;
}