// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/namedFunctionExpressionAssignedToClassProperty.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo{

       a = function bar(){

       }; // this shouldn't crash the compiler...

       

       constructor(){

       }

}
