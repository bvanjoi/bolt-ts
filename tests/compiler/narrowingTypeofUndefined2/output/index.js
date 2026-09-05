function fn(arg) {
  if (typeof arg !== 'undefined') {
    takeArray(arg);
    var n = arg;
    for ( var p of arg) {}
    var m = [...arg];
  }
  
}