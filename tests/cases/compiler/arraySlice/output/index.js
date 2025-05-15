// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/arraySlice.ts`, Apache-2.0 License
//@ run-fail
var arr;
arr.splice(1, 1);
var b = arr.splice(1, 1);
{
  var b = [];
  var a = [...b];
}