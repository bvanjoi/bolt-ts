// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/thisReferencedInFunctionInsideArrowFunction1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var foo = (dummy) => { };
function test()
{
    foo(() =>
        function () { return this; }
    );
}