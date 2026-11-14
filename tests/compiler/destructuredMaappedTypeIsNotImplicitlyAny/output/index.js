// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructuredMaappedTypeIsNotImplicitlyAny.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
function foo(key, obj) {
  var {[key]: bar} = obj;// Element implicitly has an 'any' type because type '{ [_ in T]: number; }' has no index signature.
  
  bar;// bar : any
  
  // Note: this does work:
  var lorem = obj[key];
}