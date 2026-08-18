// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuring1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo([a, b, c]) {}
function far([a, [b], [[c]]]) {}
function bar({a1, b1, c1}) {}
function baz({a2, b2: {b1, c1}}) {}