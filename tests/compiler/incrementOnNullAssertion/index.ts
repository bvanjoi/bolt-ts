// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/incrementOnNullAssertion.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface Dictionary<T> {
    [myFavouriteType: string]: T | undefined
}
const x = 'bar'
let foo: Dictionary<number> = {}
if (foo[x] === undefined) {
    foo[x] = 1
}
else {
    let nu = foo[x]
    let n = foo[x]
    foo[x]!++
    let a: string = nu
    //~^ ERROR: Type 'number' is not assignable to type 'string'.
}