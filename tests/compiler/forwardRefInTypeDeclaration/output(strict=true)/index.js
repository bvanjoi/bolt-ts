
// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forwardRefInTypeDeclaration.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@[strict=true]  compiler-options: strict=true
//@[strict=false] compiler-options: strict=false
// forward ref ignored in a typeof
var s1 = 'x';// ignored anywhere in an interface (#35947)

var s2 = 'x';// or in a type definition

var s3 = 'x';

// or in a type literal
var s4 = 'x';// or in a declared class

var s5 = 'x';// or with qualified names

class Cls2 {
  static b = 'b';
}

var obj2 = {
  d: 'd'  
};