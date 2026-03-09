// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty4.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

class Foo {
    x: number | undefined;

    constructor() {
        this.x = 5;

        this.x;    // number
        this['x']; // number
        const a: string = this.x;
        //~^ ERROR: Type 'number' is not assignable to type 'string'.
        const b: string = this['x'];
        //~^ ERROR: Type 'number' is not assignable to type 'string'.

        const key = 'x';
        this[key]; // number
        const c: string = this[key];
        //~^ ERROR: Type 'number' is not assignable to type 'string'.
    }
}
