// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/prefixUnaryOperatorsOnExportedVariables.ts`, Apache-2.0 License

//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015
//@compiler-options: module=system

export var x = false;
export var y = 1;
if (!x) {
    
}

if (+x) {
    
}

if (-x) {
    
}

if (~x) {
    
}

if (void x) {
  //~^ ERROR: This kind of expression is always falsy.
    
}

if (typeof x) {
    
}

if (++y) {
    
}