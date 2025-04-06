// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/genericUnboundedTypeParamAssignability.ts`, Apache-2.0 License

function f1<T>(o: T) {
  o.toString(); // error
  //~^ ERROR: Property 'toString' does not exist on type 'T'.
}

function f2<T extends {}>(o: T) {
  o.toString(); // no error
}

function f3<T extends Record<string, any>>(o: T) {
  o.toString(); // no error
}

function user<T>(t: T) {
  f1(t);
  f2(t); // error in strict, unbounded T doesn't satisfy the constraint
  //~^ ERROR: Argument of type 'T' is not assignable to parameter of type '{ }'.
  f3(t); // error in strict, unbounded T doesn't satisfy the constraint
  //~^ ERROR: Argument of type 'T' is not assignable to parameter of type 'Record'.
  t.toString();  // error, for the same reason as f1()
  //~^ ERROR: Property 'toString' does not exist on type 'T'.
}

