// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collisionRestParameterInType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var v1: (_i: number, ...restParameters) => void; // no error - no code gen
var v2: {
    (_i: number, ...restParameters); // no error - no code gen
    new (_i: number, ...restParameters); // no error - no code gen
    foo(_i: number, ...restParameters); // no error - no code gen
    prop: (_i: number, ...restParameters) => void; // no error - no code gen
}