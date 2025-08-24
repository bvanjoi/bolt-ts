// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/global.ts`, Apache-2.0 License

module M {
    export function f(y:number) {
        return x+y;
    }
}

var x=10;
M.f(3);

