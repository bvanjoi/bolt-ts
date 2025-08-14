// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/typeCheckTypeArgument.ts`, Apache-2.0 License

var f: <T extends UNKNOWN>() => void;
//~^ ERROR: Cannot find name 'UNKNOWN'.

interface IFoo<T extends UNKNOWN> { }
//~^ ERROR: Cannot find name 'UNKNOWN'.

class Foo<T extends UNKNOWN> { }
//~^ ERROR: Cannot find name 'UNKNOWN'.

function bar<T extends UNKNOWN>() { }
//~^ ERROR: Cannot find name 'UNKNOWN'.

class Foo2 {
    method<T extends UNKNOWN>() { }
//~^ ERROR: Cannot find name 'UNKNOWN'.
}

(<T extends UNKNOWN>(a) => { });
//~^ ERROR: Cannot find name 'UNKNOWN'.