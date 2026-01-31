// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/returnInfiniteIntersection.ts`, Apache-2.0 License

function recursive() {
    let x = <T>(subkey: T) => recursive();
    return x as typeof x & { p };
}

let result = recursive()(1)


