// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseEntityNameWithReservedWord.ts`, Apache-2.0 License
var Bool = {};
(function (Bool) {

  Bool[Bool['false'] = 0] = 'false'
})(Bool);
var x = Bool.false;