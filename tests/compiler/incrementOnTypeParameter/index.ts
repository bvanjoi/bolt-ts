// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/incrementOnTypeParameter.ts`, Apache-2.0 License

class C<T> {
    a: T;
    foo() {
        this.a++; 
        //~^ ERROR: An arithmetic operand must be of type 'any', 'number', 'bigint' or an enum type.
        for (var i: T, j = 0; j < 10; i++) { 
        //~^ ERROR: An arithmetic operand must be of type 'any', 'number', 'bigint' or an enum type.
        }
    }
}
