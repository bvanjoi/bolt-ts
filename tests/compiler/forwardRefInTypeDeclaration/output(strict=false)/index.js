// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forwardRefInTypeDeclaration.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=true,false

var s1 = 'x';
var s2 = 'x';
var s3 = 'x';

var s4 = 'x';
var s5 = 'x';
class Cls2 {
  static b = 'b';
}

var obj2 = {
  d: 'd'  
};