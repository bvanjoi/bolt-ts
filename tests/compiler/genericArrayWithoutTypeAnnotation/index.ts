// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericArrayWithoutTypeAnnotation.ts`, Apache-2.0 License

interface IFoo<T>{
}
class Bar {
    public getBar(foo: IFoo[]) {
      //~^ ERROR: Generic type 'IFoo<T>' requires 1 type argument.
    }
}

type A = A[];
const a: A = '42';
//~^ ERROR: Type 'string' is not assignable to type 'A'.