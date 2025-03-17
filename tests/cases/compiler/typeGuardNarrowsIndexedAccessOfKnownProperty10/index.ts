// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty10.ts`, Apache-2.0 License

interface Foo { bar: any; }
const bar: { [id: string]: number } = {};

(foo: Foo) => {
	bar[id]++;
	//~^ ERROR: Block-scoped variable 'id' used before its declaration.
	const id = foo.bar;
}
