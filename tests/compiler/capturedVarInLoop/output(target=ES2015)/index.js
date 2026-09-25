for ( var i = 0; i < 10; i++) {
  var str = 'x', len = str.length;
  var lambda1 = (y) => {};
  var lambda2 = () => (lambda1(len));
}