// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/typeParameterAssignmentWithConstraints.ts`, Apache-2.0 License

function f<A, B extends A>() {
  var a: A;
  var b: B;
  a = b; // Error: Can't convert B to A
}