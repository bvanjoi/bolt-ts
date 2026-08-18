// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/errorsWithInvokablesInUnions01.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface ConstructableA {
  new(): { somePropA: any };
}

interface IDirectiveLinkFn<TScope> {
    (scope: TScope): void;
}

interface IDirectivePrePost<TScope> {
    pre?: IDirectiveLinkFn<TScope>;
    post?: IDirectiveLinkFn<TScope>;
}

export let blah: IDirectiveLinkFn<number> | ConstructableA | IDirectivePrePost<number> = (x: string) => {}
//~^ ERROR: Type '(x: string) => void' is not assignable to type 'ConstructableA | IDirectiveLinkFn<number> | IDirectivePrePost<number>'.

export let ctor: IDirectiveLinkFn<number> | ConstructableA | IDirectivePrePost<number> = class {
//~^ ERROR: Type 'typeof (Anonymous class)' is not assignable to type 'ConstructableA | IDirectiveLinkFn<number> | IDirectivePrePost<number>'.
    someUnaccountedProp: any;
}