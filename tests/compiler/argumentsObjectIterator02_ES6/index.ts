// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsObjectIterator02_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es6

function doubleAndReturnAsArray(x: number, y: number, z: number): [number, number, number] {
    let blah = arguments[Symbol.iterator];

    let result = [];
    for (let arg of blah()) {
        result.push(arg + arg);
    }
    return <[any, any, any]>result;
}
