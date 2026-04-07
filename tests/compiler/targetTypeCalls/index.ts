// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/targetTypeCalls.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var fra1: (v:any)=>string = function() { return function (v:string) {return v;}; }() // should work
var fra2: (v:any)=>number = function() { return function () { return 0; } }() // should work

var fra3: (v:any)=>string = function() { return function() { return function(v) {return v;};}(); }() // should work
var fra4: (v:any)=>void = function() { return function() { return function(v) {return v;};}(); }() // should work