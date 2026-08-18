// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/exhaustiveSwitchWithWideningLiteralTypes.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class A {
  kind = 'A';
}
class B {
  kind = 'B';
}
function f(value) {
  switch (value.kind) {
    case 'A':
      return 0;
    
    case 'B':
      return 1;
    
  }
}