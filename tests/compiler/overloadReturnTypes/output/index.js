// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadReturnTypes.ts`, Apache-2.0 License
class Accessor {}
function attr(nameOrMap, value) {
  if (nameOrMap && typeof nameOrMap === 'object') {
    return new Accessor();
  } else {
    return 's';
  }
  
}