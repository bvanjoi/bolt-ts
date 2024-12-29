// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/extendsClauseAlreadySeen.ts`, Apache-2.0 License

class C {

}
class D extends C extends C { //~ ERROR: 'extends' clause already seen.
    baz() { }
}