// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/overloadGenericFunctionWithRestArgs.ts`, Apache-2.0 License

class B<V>{
  private id: V;
}
class A<U>{
  GetEnumerator: () => B<U>;
}
function Choice<T>(...v_args: T[]): A<T>;
function Choice<T>(...v_args: T[]): A<T> {
  return new A<T>();
}

var b0: B<number> = Choice('').GetEnumerator();
//~^ ERROR:  Type 'B<string>' is not assignable to type 'B<number>'.
var b1: B<number> = Choice(42).GetEnumerator();