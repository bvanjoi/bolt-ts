// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/functionType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

function salt() {}
salt.apply("hello", []);
(new Function("return 5"))();
 
 
