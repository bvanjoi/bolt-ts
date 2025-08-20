function makePoint(x) {
  return {
      get x() {
      return x
    }    
  }
}
;
var x = makePoint(2).x;
var y = makePoint(2).x;