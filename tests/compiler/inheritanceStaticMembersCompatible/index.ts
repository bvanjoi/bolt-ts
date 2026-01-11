// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/inheritanceStaticMembersCompatible.ts`, Apache-2.0 License

class a {
    static x: a;
}

class b extends a {
    static x: b;
}