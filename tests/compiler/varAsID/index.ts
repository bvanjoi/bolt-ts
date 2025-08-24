// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/varAsID.ts`, Apache-2.0 License

class Foo {
    var; // ok
    x=1;
}

var f = new Foo();


class Foo2 {
    var // not an error, because of ASI.
    x=1;
}

var f2 = new Foo2();