// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuringPropertyAssignmentNameIsNotAssignmentTarget.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function qux(bar) {
  var foo;
  ({
      value: foo    
  } = bar);
  var x = () => (bar);
}