// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeGuardNarrowsIndexedAccessOfKnownProperty3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

type Foo = (number | undefined)[] | undefined;

const foo: Foo = [1, 2, 3];
const index = 1;

if (foo !== undefined && foo[index] !== undefined && foo[index] >= 0) {
    foo[index] // number
    let a: string = foo[index];
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}
