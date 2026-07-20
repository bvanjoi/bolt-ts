// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/argumentsObjectIterator01_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function doubleAndReturnAsArray(x: number, y: number, z: number): [number, number, number] {
    let result = [];
    for (let arg of arguments) {
        result.push(arg + arg);
    }
    return <[any, any, any]>result;
}