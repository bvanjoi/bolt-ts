label1: for ( var i = 0; i < 1; i++) {
  continue label1;
}
(function () {
  b: {
    if (0) break b;
    
    (function () {
      b: {
        if (0) break b;
        
        Number('d');
      }
    }());
  }
}());
function a() {
  for ( var i = 0; i < 0; i++) {
    continue;
  }
  for ( var i = 0; i < 0; i++) {
    break;
  }
}