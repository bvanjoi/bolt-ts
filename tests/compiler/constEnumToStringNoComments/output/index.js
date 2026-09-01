var Foo = {};
(function (Foo) {

  Foo[Foo['X'] = 100] = 'X'
  Foo[Foo['Y'] = 0.5] = 'Y'
  Foo[Foo['Z'] = 2] = 'Z'
  Foo[Foo['A'] = -1] = 'A'
  Foo[Foo['B'] = -1.5] = 'B'
  Foo[Foo['C'] = -1] = 'C'
})(Foo);
var x0 = Foo.X.toString();
var x1 = Foo['X'].toString();
var y0 = Foo.Y.toString();
var y1 = Foo['Y'].toString();
var z0 = Foo.Z.toString();
var z1 = Foo['Z'].toString();
var a0 = Foo.A.toString();
var a1 = Foo['A'].toString();
var b0 = Foo.B.toString();
var b1 = Foo['B'].toString();
var c0 = Foo.C.toString();
var c1 = Foo['C'].toString();