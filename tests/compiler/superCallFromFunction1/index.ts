// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/superCallFromFunction1.ts`, Apache-2.0 License

function foo() {
    super(value => String(value));
    //~^ ERROR: Super calls are not permitted outside constructors or in nested functions inside constructors.
}