// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeAnnotationStringLiteral.ts`, Apache-2.0 License
//@[target=ES5]     compiler-options: target=ES5
//@[target=ES2015]  compiler-options: target=ES2015
//@compiler-options: declaration
function foo(a) {
  if (a === 'hello') {
    return a.length;
  }
  
  return a;
}