// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/optionalAccessorsInInterface1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

interface MyPropertyDescriptor {
    get? (): any;
    set? (v: any): void;
}

declare function defineMyProperty(o: any, p: string, attributes: MyPropertyDescriptor): any;
defineMyProperty({}, "name", { get: function () { return 5; } });

interface MyPropertyDescriptor2 {
    get?: () => any;
    set?: (v: any) => void;
}

declare function defineMyProperty2(o: any, p: string, attributes: MyPropertyDescriptor2): any;
defineMyProperty2({}, "name", { get: function () { return 5; } });
