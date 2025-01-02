// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/implementsClauseAlreadySeen.ts`, Apache-2.0 License

class C {
    
}
class D implements C implements C { //~ ERROR: 'implements' clause already seen.
    baz() { }
}