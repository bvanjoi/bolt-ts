// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/mappedTypePartialConstraints.ts`, Apache-2.0 License

interface MyInterface {
  something: number;
}

class MyClass<T extends MyInterface> {
  doIt(data : Partial<T>) {}
}

class MySubClass extends MyClass<MyInterface> {}

function fn(arg: typeof MyClass) {};

fn(MySubClass);
