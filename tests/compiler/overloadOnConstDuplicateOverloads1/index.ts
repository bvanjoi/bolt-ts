// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadOnConstDuplicateOverloads1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

function foo(a: 'hi', x: string);
function foo(a: 'hi', x: string);
function foo(a: any, x: any) {
}

function foo2(a: 'hi', x: string);
function foo2(a: 'hi', x: string);
function foo2(a: string, x: string);
function foo2(a: any, x: any) {
}