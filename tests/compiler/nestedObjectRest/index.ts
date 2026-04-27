// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nestedObjectRest.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var x, y;

[{ ...x }] = [{ abc: 1 }];
for ([{ ...y }] of [[{ abc: 1 }]]) ;

