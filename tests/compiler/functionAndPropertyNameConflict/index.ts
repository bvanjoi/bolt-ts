// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/functionAndPropertyNameConflict.ts`, Apache-2.0 License

class C65 {
    public aaaaa() { }
    public get aaaaa() { //~ ERROR: Duplicate identifier 'aaaaa'.
        return 1;
    }
}