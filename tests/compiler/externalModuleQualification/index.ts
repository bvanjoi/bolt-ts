// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/externalModuleQualification.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export var ID = "test";
export class DiffEditor<A, B, C> {
    private previousDiffAction: NavigateAction;
    //~^ ERROR: Property 'previousDiffAction' has no initializer and is not definitely assigned in the constructor.
    constructor(id: string = ID) {
    }
}
class NavigateAction {
    f(editor: DiffEditor<any, any, any>) {
    }
}
