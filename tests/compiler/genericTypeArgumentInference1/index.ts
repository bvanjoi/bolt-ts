// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/genericTypeArgumentInference1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace Underscore {
    export interface Iterator<T, U> {
        (value: T, index: any, list: any): U;
    }
    export interface Static {
        all<T>(list: T[], iterator?: Iterator<T, boolean>, context?: any): T;
        identity<T>(value: T): T;
    }
}
declare var _: Underscore.Static;

var r = _.all([true, 1, null, 'yes'], _.identity);
//~^ ERROR: Argument of type '(value: T) => T' is not assignable to parameter of type 'Iterator<false | true | number | string, boolean>'.
var r2 = _.all([true], _.identity);
var r3 = _.all([], _.identity);
var r4 = _.all([<any>true], _.identity);
