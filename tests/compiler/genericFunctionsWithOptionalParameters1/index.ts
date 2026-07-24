// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctionsWithOptionalParameters1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Utils {
   fold<T, S>(c?: Array<T>, folder?: (s: S, t: T) => T, init?: S): T;
}

var utils: Utils;

utils.fold(); // no error
//~^ ERROR: Variable 'utils' is used before being assigned.
utils.fold(null); // no error
//~^ ERROR: Variable 'utils' is used before being assigned.
//~| ERROR: Argument of type 'null' is not assignable to parameter of type 'undefined | unknown[]'.
utils.fold(null, null); // no error
//~^ ERROR: Variable 'utils' is used before being assigned.
//~| ERROR: Argument of type 'null' is not assignable to parameter of type 'undefined | unknown[]'.
utils.fold(null, null, null); // no error
//~^ ERROR: Variable 'utils' is used before being assigned.
//~| ERROR: Argument of type 'null' is not assignable to parameter of type 'undefined | unknown[]'.
