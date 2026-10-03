// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/useBeforeDeclaration_propertyAssignment.ts`, Apache-2.0 License

//@compiler-options: target=ES6

export class C {
    public a =  { b: this.b, ...this.c, [this.b]: `${this.c}`};
    //~^ ERROR: Property 'b' is used before its initialization.
    //~| ERROR: Property 'b' is used before its initialization.
    //~| ERROR: Property 'b' is used before its initialization.
    //~| ERROR: Property 'c' is used before its initialization.
    //~| ERROR: Property 'c' is used before its initialization.
    //~| ERROR: Property 'c' is used before its initialization.
    private b = 0;
    public c = { c: this.b };
}

class D {
    static A = class extends D.B {
    //~^ ERROR: Property 'B' is used before its initialization.
        [D.D]() {} // should be an error
    }
    static B = class {}
    static C = {
        [D.D]: 1,
    //~^ ERROR: Property 'D' is used before its initialization.
    //~| ERROR: Property 'D' is used before its initialization.
        ...{get [D.D]() {return 0;}} // should be an error
    };
    static D = '';
}
