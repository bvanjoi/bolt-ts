// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/indexClassByNumber.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class foo { }

var f = new foo();

f[0] = 4; // Shouldn't be allowed