// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/functionAssignmentError.ts`, Apache-2.0 License
var func = function () {
  return "ONE"
};
func = function () {
  return "ONE"
};