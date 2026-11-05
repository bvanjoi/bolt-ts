// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnObjectLiteral4.ts`, Apache-2.0 License

//@compiler-options: removeComments=false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var v = {
 /**
  * @type {number}
  */
 get bar(): number {
  return 12;
 }
}
