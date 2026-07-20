// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/overloadGenericFunctionWithRestArgs.ts`, Apache-2.0 License

class B<V>{
  private id: V;
  //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
}
class A<U>{
  GetEnumerator: () => B<U>;
  //~^ ERROR: Property 'GetEnumerator' has no initializer and is not definitely assigned in the constructor.
}
function Choice<T>(...v_args: T[]): A<T>;
function Choice<T>(...v_args: T[]): A<T> {
  return new A<T>();
}

var b0: B<number> = Choice('').GetEnumerator();
//~^ ERROR:  Type 'B<string>' is not assignable to type 'B<number>'.
var b1: B<number> = Choice(42).GetEnumerator();
