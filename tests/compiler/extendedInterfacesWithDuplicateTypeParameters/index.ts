// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/extendedInterfacesWithDuplicateTypeParameters.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface InterfaceWithMultipleTypars<A, A> { // should error
  //~^ ERROR: Duplicate identifier 'A'.
	bar(): void;
}

interface InterfaceWithSomeTypars<B> { // should not error
	//~^ ERROR: All declarations of 'InterfaceWithSomeTypars' must have identical type parameters.
	bar(): void;
}

interface InterfaceWithSomeTypars<C, C> { // should error
  //~^ ERROR: Duplicate identifier 'C'.
	//~| ERROR: All declarations of 'InterfaceWithSomeTypars' must have identical type parameters.
	bar2(): void;
}