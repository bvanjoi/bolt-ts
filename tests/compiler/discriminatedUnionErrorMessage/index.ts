// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminatedUnionErrorMessage.ts`, Apache-2.0 License

type Square = { kind: "sq", size: number }
type Rectangle = { kind: "rt", x: number, y: number }
type Circle = { kind: "cr", radius: number }
type Shape =
    | Square
    | Rectangle
    | Circle;
let shape: Shape = {
    kind: "sq",
    x: 12,
    //~^ ERROR: Object literal may only specify known properties, and 'x' does not exist in type 'Square'.
    y: 13,
}

