// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultIndexProps2.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Foo {
  v = 'Yo';
}
var f = new Foo();
var o = {
  v: 'Yo2'  
};
1[0];
var q = 's'[0];