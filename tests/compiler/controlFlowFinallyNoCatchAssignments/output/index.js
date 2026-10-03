var x;
x = Math.random();
var a;
try {
  if (x) {
    a = 1;
  } else {
    a = 2;
  }
  
}finally {
  console.log(x);
}
console.log(a);