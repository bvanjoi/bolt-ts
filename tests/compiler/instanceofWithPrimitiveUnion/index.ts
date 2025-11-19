// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/instanceofWithPrimitiveUnion.ts`, Apache-2.0 License

//@compiler-options: strict

function test1(x: number | string) {
    if (x instanceof Object) {
      //~^ ERROR: The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter.
        x;
    }
}

function test2(x: (number | string) | number) {
    if (x instanceof Object) {
      //~^ ERROR: The left-hand side of an 'instanceof' expression must be of type 'any', an object type or a type parameter.
        x;
    }
}
