// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/assignmentToFunction.ts`, Apache-2.0 License

function fn() { }
fn = () => 3;
//~^ ERROR: Cannot assign to 'fn' because it is a function.
//~| ERROR: Cannot assign to 'fn' because it is a function.

module foo {
    function xyz() {
        function bar() {
        }
        bar = null;
        //~^ ERROR: Cannot assign to 'bar' because it is a function.
    }
}