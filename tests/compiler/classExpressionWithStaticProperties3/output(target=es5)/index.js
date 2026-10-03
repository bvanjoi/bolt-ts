
var arr = [];
for ( var i = 0; i < 3; i++) {
  arr.push(class C {
    static x = i;
    static y = () => (C.x * 2);
  });
}
arr.forEach((C) => (console.log(C.y())));