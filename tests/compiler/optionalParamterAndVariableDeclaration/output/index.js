// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalParamterAndVariableDeclaration.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class C {
  constructor(options) {var options = (options || 0);}
}