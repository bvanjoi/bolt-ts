// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceMergeWithNonGenericTypeArguments.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export class SomeBaseClass { }
export interface SomeInterface { }
export interface MergedClass extends SomeInterface { }
export class MergedClass extends SomeBaseClass<any> {
  //~^ ERROR: Type 'SomeBaseClass' is not generic.
	public constructor() {
		super();
    //~^ ERROR: Call target does not contain any signatures.
	}
}