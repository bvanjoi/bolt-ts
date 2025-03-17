// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/cannotInvokeNewOnErrorExpression.ts`, Apache-2.0 License

module M
{
    class ClassA {}
}
var t = new M.ClassA[];
//~^ ERROR: An element access expression should take an argument.
//~| ERROR:  Property 'ClassA' does not exist on type 'typeof M'.