class a {
  static b
}
var b = (a);
var b = (a).b;
var b = (a.b).c;
var b = (a.b()).c;
var b = (new a());
var b = (new a.b());
var b = (new a()).b;