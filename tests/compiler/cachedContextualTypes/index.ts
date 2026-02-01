// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/cachedContextualTypes.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=es2015
//@run-fail

// Repro from #52198

declare function createInstance<Ctor extends new (...args: any[]) => any, R extends InstanceType<Ctor>>(ctor: Ctor, ...args: ConstructorParameters<Ctor>): R;

export interface IMenuWorkbenchToolBarOptions {
    toolbarOptions: {
        foo(bar: string): string
    };
}

class MenuWorkbenchToolBar {
    constructor(
        options: IMenuWorkbenchToolBarOptions | undefined,
    ) { }
}

createInstance(MenuWorkbenchToolBar, {
    toolbarOptions: {
        foo(bar) { return bar; }
    }
});