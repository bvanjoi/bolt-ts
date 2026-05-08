function flatMapChildren(node, cb) {
  var result = [];
  node.forEachChild((child) => {
    var value = cb(child);
    if (value !== undefined) {
      result.push(...toArray(value));
    }
    
  });
  return result
}
function flatMapChildren2(node, cb) {
  var result = [];
  node.forEachChild((child) => {
    var value = cb(child);
    if (value !== null) {
      result.push(...toArray(value));
    }
    
  });
  return result
}