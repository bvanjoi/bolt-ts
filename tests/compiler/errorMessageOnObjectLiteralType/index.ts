// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/errorMessageOnObjectLiteralType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare var x: {
    a: string;
    b: number;
};
x.getOwnPropertyNamess();
//~^ ERROR: Property 'getOwnPropertyNamess' does not exist on type '{ a: string; b: number; }'.
Object.getOwnPropertyNamess(null);
//~^ ERROR: Property 'getOwnPropertyNamess' does not exist on type 'ObjectConstructor'.