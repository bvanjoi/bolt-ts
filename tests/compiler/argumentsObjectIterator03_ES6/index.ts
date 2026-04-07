// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/argumentsObjectIterator03_ES6.ts`, Apache-2.0 License

//@compiler-options: target=es6

function asReversedTuple(a: number, b: string, c: boolean): [boolean, string, number] {
    let [x, y, z] = arguments;
    
    return [z, y, x];
}
