
// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/objectCreate-errors.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@run-fail
var n = Object.create(null);
var t = Object.create({
  a: 1,
  b: ''  
}// object
);
var u = Object.create(union);
var e = Object.create// {a: number, b: string }
({});
var o = Object.// object | {a: number, b: string }
create(// {}
({}));// object

var a = Object.create(null, {});// any

var a = Object.create({
  a: 1,
  b: ''  
}, {});
var a = Object.create(union, {});
var a = Object.create({}, {});
var a = Object.create(({}), {});