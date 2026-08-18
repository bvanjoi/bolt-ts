// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/narrowingTypeofUndefined2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function fn(arg) {
  if (typeof arg !== 'undefined') {
    takeArray(arg);
    var n = arg;
    for ( var p of arg) {}
    var m = [...arg];
  }
  
}