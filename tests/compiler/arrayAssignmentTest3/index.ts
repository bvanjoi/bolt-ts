// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/arrayAssignmentTest3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class B {}

class a {
    constructor(public x: string, public y: number, z: B[]) { }
}



var xx = new a(null, 7, new B());
//~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'string'.
