// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/bigIntWithTargetLessThanES2016.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[esnext]

BigInt(1) ** BigInt(1); // should not error
//~^ ERROR: Exponentiation cannot be performed on 'bigint' values unless the 'target' option is set to 'es2016' or later.

let num = BigInt(2);
num **= BigInt(2); // should not error
//~^ ERROR: Exponentiation cannot be performed on 'bigint' values unless the 'target' option is set to 'es2016' or later.
