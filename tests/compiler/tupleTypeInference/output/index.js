// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/tupleTypeInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail

// Implicit different types
var a = $q.all([$q.when(), $q.when()]);
var b = $q.all// Explicit different types
([$q.when(), $q.when()]);
var c = $q.all([$q.when(), $q.when// Implicit identical types
()]);