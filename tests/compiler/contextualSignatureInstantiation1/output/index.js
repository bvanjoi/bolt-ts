// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualSignatureInstantiation1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@run-fail
var e = (x, y) => (x.length);
var r99 = map(e);// should be {}[] for S since a generic lambda is not inferentially typed

var e2 = (x, y) => (x.length);
var r100 = map2(e2);