// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/interfaceContextualType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export interface IOptions {
    italic?: boolean;
    bold?: boolean;
}
export interface IMap {
    [s: string]: IOptions;
}

class Bug {
    public values: IMap;
    //~^ ERROR: Property 'values' has no initializer and is not definitely assigned in the constructor.
    ok() {
        this.values = {};
        this.values['comments'] = { italic: true };
    }
    shouldBeOK() {
        this.values = {
            comments: { italic: true }
        };
    }
}