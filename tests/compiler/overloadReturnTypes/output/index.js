// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadReturnTypes.ts`, Apache-2.0 License
//@compiler-options: target=es2015
class Accessor {}
function attr(nameOrMap, value) {
  if (nameOrMap && typeof nameOrMap === 'object') {
    // handle map case
    return new Accessor();
  } else {
    // handle string case
    return 's';
  }
  
}