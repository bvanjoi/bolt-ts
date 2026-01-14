// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminatedUnionErrorMessage.ts`, Apache-2.0 License

type Square = { kind: "sq", size: number }
type Rectangle = { kind: "rt", x: number, y: number }
type Circle = { kind: "cr", radius: number }
type Shape =
    | Square
    | Rectangle
    | Circle;
let shape: Shape = {
//~^ ERROR: Type '{ kind: "sq"; x: number; y: number; }' is not assignable to type '{ kind: "sq"; size: number; } | { kind: "rt"; x: number; y: number; } | { kind: "cr"; radius: number; }'.
    kind: "sq",
    x: 12,
    y: 13,
}

