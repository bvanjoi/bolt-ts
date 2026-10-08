// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/newLexicalEnvironmentForConvertedLoop.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//compiler-options: noImplicitAny
function baz(x) {
  return [[x, x]];
}
function foo(set) {
  for ( var [value, i] of baz(set.values)) {
    var bar = [];
    (() => (bar));
    set.values.push(...[]);
  }
}
;