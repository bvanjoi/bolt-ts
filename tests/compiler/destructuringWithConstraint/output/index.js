// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/destructuringWithConstraint.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function foo(props) {
  var {foo = false} = props;
  if (foo === true) {}
  
}