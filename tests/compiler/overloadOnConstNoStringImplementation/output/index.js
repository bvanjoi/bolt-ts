function x2(a, cb) {
  cb('hi');
  cb('bye');
  var hm = 'hm';
  cb(hm);
  cb('uh');
  cb(1);
}
var cb = (x) => (1);
x2(1, cb);
x2(1, (x) => (1));
x2(1, (x) => (1));