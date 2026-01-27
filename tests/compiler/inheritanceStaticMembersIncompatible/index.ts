// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritanceStaticMembersIncompatible.ts`, Apache-2.0 License

class a {
    static x: string;
}

class b extends a {
//~^ ERROR: Class static side 'typeof b' incorrectly extends base class static side 'typeof a'.
    static x: number;
}