// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/argumentsObjectIterator03_ES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function asReversedTuple(a: number, b: string, c: boolean): [boolean, string, number] {
    let [x, y, z] = arguments;
    //~[target=ES5]^      ERROR: Type 'IArguments' is not an array type.
    //~[target=ES5]|      ERROR: Type 'IArguments' is not an array type.
    //~[target=ES5]|      ERROR: Type 'IArguments' is not an array type.
    return [z, y, x];
}

