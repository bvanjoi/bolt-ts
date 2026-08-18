// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declInput.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
class bar {
  f() {
    return '';
  }
  g() {
    return {
          a: null,
      b: undefined,
      c: void 4      
    };
  }
  h(x = 4, y = null, z = '') {
    x++;
  }
}