// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeParameterFixingWithConstraints.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false
//@run-fail

interface IBar {
    [barId: string]: any;
}

interface IFoo {
    foo<TBar extends IBar>(bar: TBar, bar1: (bar: TBar) => TBar, bar2: (bar: TBar) => TBar): TBar;
}

var foo: IFoo;
foo.foo({ bar: null }, bar => null, bar => null);