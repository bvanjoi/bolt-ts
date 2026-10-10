// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/argumentsObjectIterator01_ES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

function doubleAndReturnAsArray(x: number, y: number, z: number): [number, number, number] {
    let result = [];
    for (let arg of arguments) {
      //~[target=ES5]^      ERROR: Type 'IArguments' is not an array type.
        result.push(arg + arg);
    }
    return <[any, any, any]>result;
}