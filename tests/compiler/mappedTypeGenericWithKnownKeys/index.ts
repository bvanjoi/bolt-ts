// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/mappedTypeGenericWithKnownKeys.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

function test<Shape extends Record<string, string>>(shape: Shape, key: keyof Shape) {
    const obj = {} as Record<keyof Shape | "knownLiteralKey", number>;

    obj.knownLiteralKey = 1;
    obj[key] = 2;

    obj.unknownLiteralKey = 3; // error
    //~^ ERROR: Property 'unknownLiteralKey' does not exist on type 'Record<keyof Shape | "knownLiteralKey", number>'.
    obj['' as string] = 4; // error
    //~^ ERROR: Type 'Record<keyof Shape | "knownLiteralKey", number>' is generic and can only be indexed for reading.
}
