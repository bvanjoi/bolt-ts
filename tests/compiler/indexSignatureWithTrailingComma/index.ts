// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/indexSignatureWithTrailingComma.ts`, Apache-2.0 License

type A = {
    [key: string,]: string;
    //~^ ERROR: An index signature cannot have a trailing comma.
};

interface B {
    [key: string,]: string;
    //~^ ERROR: An index signature cannot have a trailing comma.
}

class C {
    [key: string,]: null;
    //~^ ERROR: An index signature cannot have a trailing comma.
}