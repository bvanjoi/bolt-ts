// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLiteralPropertyImplicitlyAny.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: noImplicitAny
var foo = Symbol.for('foo');
var o = {
  [foo]: undefined  
};