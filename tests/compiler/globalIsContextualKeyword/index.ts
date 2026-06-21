// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/v.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function a() {
    let global = 1;
}
function b() {
    class global {}
}

namespace global {
}

function foo(global: number) {
}

let obj = {
    global: "123"
}