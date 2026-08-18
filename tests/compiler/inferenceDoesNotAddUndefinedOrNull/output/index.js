// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/inferenceDoesNotAddUndefinedOrNull.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function flatMapChildren(node, cb) {
  var result = [];
  node.forEachChild((child) => {
    var value = cb(child);
    if (value !== undefined) {
      result.push(...toArray(value));
    }
    
  });
  return result;
}
function flatMapChildren2(node, cb) {
  var result = [];
  node.forEachChild((child) => {
    var value = cb(child);
    if (value !== null) {
      result.push(...toArray(value));
    }
    
  });
  return result;
}