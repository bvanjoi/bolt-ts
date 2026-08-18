// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typeofEnum.ts`, Apache-2.0 License

//@compiler-options: target=es2015

enum E {
    e1,
    e2
}

var e1: typeof E;
e1.e1;
//~^ ERROR: Variable 'e1' is used before being assigned.