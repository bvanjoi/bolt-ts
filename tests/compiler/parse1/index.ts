// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/parse1.ts`, Apache-2.0 License
var bar = 42;
function foo() {
 bar.
}
//~^ ERROR: Identifier expected.
