// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/extendedInterfaceGenericType.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Alpha<T> {
    takesArgOfT(arg: T): Alpha<T>;
    makeBetaOfNumber(): Beta<number>;
}
interface Beta<T> extends Alpha<T> {
}

var alpha: Alpha<number>;
var betaOfNumber = alpha.makeBetaOfNumber();
//~^ ERROR: Variable 'alpha' is used before being assigned.
//~| ERROR: Variable 'alpha' is used before being assigned.
betaOfNumber.takesArgOfT(5);
