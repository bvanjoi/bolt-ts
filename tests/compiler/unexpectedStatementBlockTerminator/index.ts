// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/unexpectedStatementBlockTerminator.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Foo {}

class Bar {}
case //~ERROR: Declaration or statement expected.

function Goo() {return {a:1,b:2};}
