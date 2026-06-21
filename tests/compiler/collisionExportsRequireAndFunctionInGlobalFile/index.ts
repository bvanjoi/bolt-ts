// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionExportsRequireAndFunctionInGlobalFile.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function exports() {
    return 1;
}
function require() {
    return "require";
}
namespace m3 {
    function exports() {
        return 1;
    }
    function require() {
        return "require";
    }
}
namespace m4 {
    export function exports() {
        return 1;
    }
    export function require() {
        return "require";
    }
}