// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/extendsClauseAlreadySeen2.ts`, Apache-2.0 License

class C<T> {

}
class D<T> extends C<number> extends C<string> { //~ ERROR: 'extends' clause already seen.
    baz() { }
}