// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/functionWithSameNameAsField.ts`, Apache-2.0 License

class TestProgressBar {
    public total: number;
    public total(total: number) {
    //~^ ERROR: Duplicate identifier 'total'.
        this.total = total;
        return this;
    }
}
