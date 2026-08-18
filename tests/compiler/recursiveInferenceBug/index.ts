// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/recursiveInferenceBug.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function f(x: number) {
    var z = f(x);
    return x;
}


var zz = {
    g: () =>{ },
    get f() { return "abc"; },
};