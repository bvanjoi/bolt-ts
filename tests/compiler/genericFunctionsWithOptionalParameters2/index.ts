// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericFunctionsWithOptionalParameters2.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Utils {
   fold<T, S>(c: Array<T>, folder?: (s: S, t: T) => T, init?: S): T;
}

declare var utils: Utils;

utils.fold(); // error
//~^ ERROR: Expected 1-3 arguments, but got 0.
utils.fold(null); // no error
//~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'unknown[]'.
utils.fold(null, null); // no error
//~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'unknown[]'.
utils.fold(null, null, null); // error: Unable to invoke type with no call signatures
//~^ ERROR: Argument of type 'null' is not assignable to parameter of type 'unknown[]'.
