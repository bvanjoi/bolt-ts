// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericTemplateOverloadResolution.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface IFooFn {
    (strings: TemplateStringsArray): Promise<{}>;
    <T>(strings: TemplateStringsArray): Promise<T>;
}

declare const fooFn: IFooFn;

declare function expect(x: Promise<number>): void;

expect(fooFn<number>``);
