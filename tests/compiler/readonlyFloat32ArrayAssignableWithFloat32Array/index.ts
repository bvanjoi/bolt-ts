// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/readonlyFloat32ArrayAssignableWithFloat32Array.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

function update(b: Readonly<Float32Array>) {
    const c = copy(b);
    add(c, c);
}

function add(a: Float32Array, b: Float32Array, c: Float32Array = a) {
    c[0] = a[0] + b[0];
}

function copy(a: Float32Array) {
    return new Float32Array(a);
}