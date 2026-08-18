// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonMergedDeclarationsAndOverloads.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class A {
    m1: string;
    //~^ ERROR: Property 'm1' has no initializer and is not definitely assigned in the constructor.
    f() {}
    m1 (a: string): void;
    //~^ ERROR: Duplicate identifier 'm1'.
    m1 (a: number): void;
    //~^ ERROR: Duplicate identifier 'm1'.
    m1 (a: any): void {
    //~^ ERROR: Duplicate identifier 'm1'.
    }
}