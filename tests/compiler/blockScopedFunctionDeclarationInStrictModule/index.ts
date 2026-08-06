// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/blockScopedFunctionDeclarationInStrictModule.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: module=commonjs


if (true) {
    function foo() { }
    foo(); // ok
}

export = foo; // not ok
//~^ ERROR: Cannot find name 'foo'.