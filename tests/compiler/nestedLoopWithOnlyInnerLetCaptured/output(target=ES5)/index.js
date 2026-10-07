
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedLoopWithOnlyInnerLetCaptured.ts`, Apache-2.0 License
//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
for ( var a1 of []) for ( var a2 of a1.someArray) doSomething(() => (a2));