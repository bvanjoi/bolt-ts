// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadResolutionWithAny.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
var func;
func('');
func(3);
var x;
func(x);
var func2;
func2(x, x);
func2('', '');
func2(x, '');
func2('', x);