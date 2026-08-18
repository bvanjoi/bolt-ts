// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/restUnion2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks

var rest2;
var {...rest2} = undefinedUnion;

var rest3;
var {...rest3} = nullUnion;