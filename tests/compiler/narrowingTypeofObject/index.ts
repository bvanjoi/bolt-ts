// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingTypeofObject.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=true

interface F { (): string }

function test(x: number & { _foo: string }) {
    if (typeof x === 'object') {
        x; 
    }
}

function f1(x: F & { foo: number }) {
    if (typeof x !== "object") {
        x;
    }
}