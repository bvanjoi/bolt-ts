// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/superHasMethodsFromMergedInterface.ts`, Apache-2.0 License

class C { m1() { } }
interface C { m2(): void }
class Sub extends C {
    m3() {
        super.m2();
        let m1 = super.m1();
        let m2: string = super.m2();
        //~^ ERROR: Type 'void' is not assignable to type 'string'.
        let m3 = super.m3();
        //~^ ERROR: Property 'm3' does not exist on type 'C<Sub>'.
    }
}
