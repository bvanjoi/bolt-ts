// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalTypeRelaxingConstraintAssignability.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict

export class Elem {
  constructor(children_) {}
}
new Elem(undefined);
new Elem('');
new Elem('');
// error
new Elem('');// error
// Repro from #31766

function g(p1, p2) {
  return f(p1, p2);
}