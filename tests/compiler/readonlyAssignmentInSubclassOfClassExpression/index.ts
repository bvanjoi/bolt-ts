// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/readonlyAssignmentInSubclassOfClassExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class C extends (class {} as new () => Readonly<{ attrib: number }>) {
    constructor() {
        super()
        this.attrib = 2
        //~^ ERROR: Cannot assign to 'attrib' because it is a read-only property.
    }
}