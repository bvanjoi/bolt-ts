// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitDestructuring1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

function foo([a, b, c]: [string, string, string]): void { }
function far([a, [b], [[c]]]: [number, boolean[], string[][]]): void { }
function bar({a1, b1, c1}: { a1: number, b1: boolean, c1: string }): void { }
function baz({a2, b2: {b1, c1}}: { a2: number, b2: { b1: boolean, c1: string } }): void { } 
