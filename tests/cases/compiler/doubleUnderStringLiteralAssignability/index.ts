// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/doubleUnderStringLiteralAssignability.ts`, Apache-2.0 License

var shouldBeOk: '__dunder' = '__dunder';
var bad: '__dunder' = 'no_dunder';
//~^ ERROR: Type '"no_dunder"' is not assignable to type '"__dunder"'.
var okok: '___thunder' = '___thunder';
var alsoOk: '_sunder' = '_sunder';
