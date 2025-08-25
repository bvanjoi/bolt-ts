// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/validRegexp.ts`, Apache-2.0 License

var x = / [a - z /]$ / i;
//~^ ERROR: Expected ','.
var x1 = /[a-z/]$/i;
var x2 = /[a-z/]$ /i;