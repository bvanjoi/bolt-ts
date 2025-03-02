// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/assignmentCompatability45.ts`, Apache-2.0 License

abstract class A {}
class B extends A {
    constructor(x: number) {
        super();
    }
}
const b: typeof A = B;
//~^ ERROR: Type 'typeof B' is not assignable to type 'typeof A'.