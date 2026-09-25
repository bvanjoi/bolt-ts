// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/bigIntWithTargetES2016.ts`, Apache-2.0 License

//@compiler-options: target=es2016
//@compiler-options: lib=[esnext]

BigInt(1) ** BigInt(1); // should not error

let num = BigInt(2);
num **= BigInt(2); // should not error
