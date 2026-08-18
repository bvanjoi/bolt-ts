// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/overloadCallTest.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class foo {
  constructor() {function bar(foo) {
      return 'foo';
    }
    ;
    var test = bar('test');
    var goo = bar();
    goo = bar('test');}
}