// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/privatePropertyUsingObjectType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=amd

export class FilterManager {
    private _filterProviders: { index: IFilterProvider; };
    //~^ ERROR: Property '_filterProviders' has no initializer and is not definitely assigned in the constructor.
    private _filterProviders2: { [index: number]: IFilterProvider; };
    //~^ ERROR: Property '_filterProviders2' has no initializer and is not definitely assigned in the constructor.
    private _filterProviders3: { (index: number): IFilterProvider; };
    //~^ ERROR: Property '_filterProviders3' has no initializer and is not definitely assigned in the constructor.
    private _filterProviders4: (index: number) => IFilterProvider;
    //~^ ERROR: Property '_filterProviders4' has no initializer and is not definitely assigned in the constructor.
}
export interface IFilterProvider {
}
