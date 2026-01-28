// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/genericClassInheritsConstructorFromNonGenericClass.ts`, Apache-2.0 License

class A extends B<string> { }
//~^ ERROR: Class 'B' used before its declaration
class B<U> extends C { }
//~^ ERROR: Class 'C' used before its declaration
class C {
    constructor(p: string) { }
}