// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/duplicateIdentifierDifferentModifiers.ts`, Apache-2.0 License

class C {
    ["a"]: string;
    ["a"]: string;
    //~^ ERROR: Duplicate identifier 'a'.
}


class D {
    a: string;
    a: string;
    //~^ ERROR: Duplicate identifier 'a'.
}