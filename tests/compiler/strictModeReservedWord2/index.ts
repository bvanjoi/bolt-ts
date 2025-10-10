// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictModeReservedWord2.ts`, Apache-2.0 License

"use strict"
interface public { }
//~^ ERROR: Identifier expected. 'public' is a reserved word in strict mode.
interface implements {
//~^ ERROR: Identifier expected. 'implements' is a reserved word in strict mode.
    foo(package, protected);
    //~^ ERROR: Identifier expected. 'package' is a reserved word in strict mode.
    //~| ERROR: Identifier expected. 'protected' is a reserved word in strict mode.
}
enum package { }
//~^ ERROR: Identifier expected. 'package' is a reserved word in strict mode.
enum foo {
    public,
    private,
    pacakge
}

const enum private {
//~^ ERROR: Identifier expected. 'private' is a reserved word in strict mode.
    public,
    private,
    pacakge
}

const enum bar {
    public,
    private,
    pacakge
}
