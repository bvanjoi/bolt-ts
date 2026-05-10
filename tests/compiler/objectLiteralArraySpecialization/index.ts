// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectLiteralArraySpecialization.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

declare function create<T>(initialValues?: T[]): MyArrayWrapper<T>;
interface MyArrayWrapper<T> {
	constructor(initialItems?: T[]);
	doSomething(predicate: (x: T, y: T) => boolean): void;
}
var thing = create([ { name: "bob", id: 24 }, { name: "doug", id: 32 } ]); // should not error
thing.doSomething((x, y) => x.name === "bob"); // should not error
