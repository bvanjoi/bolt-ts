// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constructorArgWithGenericCallSignature.ts`, Apache-2.0 License

module Test {
  export interface MyFunc {
      <T>(value1: T): T;
  }
  export class MyClass {
      constructor(func: MyFunc) { }
  }

export function F(func: MyFunc) { }
}
var func: Test.MyFunc;
Test.F(func); // OK
var test = new Test.MyClass(func); // Should be OK


class B<T> {
  constructor(x: T) {}
  f(x: T) {}
}

new B(''); 
new B(42);
new B(false);

new B<string>('');
new B<number>(42);
new B<boolean>(false);

(new B('')).f(42)
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'string'.
(new B('')).f('42')
