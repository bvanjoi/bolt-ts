// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowedConstInMethod.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function f() {
  var x = ({});
  if (x !== null) {
    return {
          bar() {
        return x.length;
      }      
    };
  }
  
}
function f2() {
  var x = ({});
  if (x !== null) {
    return class {
      bar() {
        return x.length;
      }
    };
  }
  
}