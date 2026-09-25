// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/propertyParameterWithQuestionMark.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C {
    constructor(public x?) { }
}

// x should be an optional property
var v: C = {}; // Should succeed
declare var v2: { x? }
v = v2; // Should succeed
var v3: { x } = new C; // Should fail
//~^ ERROR: Type 'C' is not assignable to type '{ x: any; }'.