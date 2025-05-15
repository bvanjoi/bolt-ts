// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/moduleRedifinitionErrors.ts`, Apache-2.0 License
class A {}

(function (A) {

})(A);