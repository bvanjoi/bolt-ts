// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralComputedNameNoDeclarationError.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: declaration
var Foo = {
  BANANA: 'banana'  
};
var Baa = {
  [Foo.BANANA]: 1  
};