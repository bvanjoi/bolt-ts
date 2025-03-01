// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/overloadingOnConstantsInImplementation.ts`, Apache-2.0 License

function foo(a: 'hi', x: string);
function foo(a: 'hi', x: string);
function foo(a: 'hi', x: any) {
}