// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/capturedVarInLoop.ts`, Apache-2.0 License

//@compiler-options: strict=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

for (var i = 0; i < 10; i++) {
    var str = 'x', len = str.length;
    let lambda1 = (y) => { };
    let lambda2 = () => lambda1(len);
}