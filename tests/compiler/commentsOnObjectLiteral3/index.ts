// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/commentsOnObjectLiteral3.ts`, Apache-2.0 License

//@compiler-options: removeComments: false
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

var v = {
 //property
 prop: 1 /* multiple trailing comments */ /*trailing comments*/,
 //property
 func: function () {
 },
 //PropertyName + CallSignature
 func1() { },
 //getter
 get a() {
  return this.prop;
 } /*trailing 1*/,
 //setter
 set a(value) {
  this.prop = value;
 } // trailing 2
};