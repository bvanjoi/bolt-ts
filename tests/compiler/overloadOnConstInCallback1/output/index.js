class C {
  x1(a, callback) {
    callback('hi');
    callback('bye');
    var hm = 'hm';
    callback(hm);
  }
}