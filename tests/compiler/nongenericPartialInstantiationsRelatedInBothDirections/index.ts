// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nongenericPartialInstantiationsRelatedInBothDirections.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface Foo {
    a: number;
    b: number;
    bar: string;
}
interface ObjectContaining<T> {
  new (sample: Partial<T>): Partial<T>
}
declare let cafoo: ObjectContaining<{ a: number, foo: number }>;
declare let cfoo: ObjectContaining<Foo>;
cfoo = cafoo;
cafoo = cfoo;
