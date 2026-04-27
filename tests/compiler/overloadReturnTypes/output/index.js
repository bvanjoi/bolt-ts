class Accessor {}
function attr(nameOrMap, value) {
  if (nameOrMap && typeof nameOrMap === 'object') {
    return new Accessor()
  } else {
    return 's'
  }
  
}