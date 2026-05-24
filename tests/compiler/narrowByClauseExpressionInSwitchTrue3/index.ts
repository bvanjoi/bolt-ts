// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue3.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/55986

type Shape =
    | { kind: "circle", radius: number }
    | { kind: "square", sideLength: number }

function wat(shape: Shape) {
    switch (true) {
        case shape.kind === "circle":
            return Math.PI * shape.radius ** 2;
        case shape.kind === "circle": // should error
    }

    if (shape.kind === "circle") {      //~ERROR: This comparison appears to be unintentional because the types '"square"' and '"circle"' have no overlap.
        return Math.PI * shape.radius ** 2;
        //~^ ERROR: Property 'radius' does not exist on type 'never'.
    }
    else if (shape.kind === "circle") { //~ERROR: This comparison appears to be unintentional because the types '"square"' and '"circle"' have no overlap.
        //         ~~~~
        // Property 'kind' does not exist on type 'never'.
    }
}