// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/tupleTypeInference.ts`, Apache-2.0 License
//@compiler-options: target=es2015

var a = $q.all([$q.when(), $q.when()]);
var b = $q.all([$q.when(), $q.when()]);
var c = $q.all([$q.when(), $q.when()]);