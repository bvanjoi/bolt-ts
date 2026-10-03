// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorInAmbientContextES5.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

// Should allow accessor in ambient contexts even when targeting ES5

declare class AmbientClass {
    accessor prop1: string;
    static accessor prop2: number;
    private accessor prop3: boolean;
    private static accessor prop4: symbol;
}

declare namespace AmbientNamespace {
    class C {
        accessor prop: string;
    }
}

// Should also work in .d.ts files (simulated with declare)
declare module "some-module" {
    export class ExportedClass {
        accessor value: any;
    }
}

// Regular class should still error when targeting ES5
class RegularClass {
    accessor shouldError: string; // Should still error
    //~^ ERROR: Property 'shouldError' has no initializer and is not definitely assigned in the constructor.
    //~[target=ES5]^^ ERROR: Properties with the 'accessor' modifier are only available when targeting ECMAScript 2015 and higher.
}