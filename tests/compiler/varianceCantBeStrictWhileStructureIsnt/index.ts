// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/varianceCantBeStrictWhileStructureIsnt.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: strictFunctionTypes=false
//@run-fail


interface Foo<T> {
    member: (cb: T) => void;
}

interface Bar<T> {
    member: (cb: T) => void;
}

declare var a: Foo<string>;
declare var b: Foo<"">;

declare var a2: Bar<string>;
declare var b2: Bar<"">;

a = b;
b = a;

a2 = b2;
b2 = a2;

a = b2;
b = a2;

a2 = b;
b2 = a;