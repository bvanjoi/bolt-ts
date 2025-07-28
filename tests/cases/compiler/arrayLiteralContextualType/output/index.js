class Giraffe {
  name = 'Giraffe'
  neckLength = '3m'
}
class Elephant {
  name = 'Elephant'
  trunkDiameter = '20cm'
}
function foo(animals) {}
function bar(animals) {}
foo([new Giraffe(), new Elephant()]);
bar([new Giraffe(), new Elephant()]);
var arr = [new Giraffe(), new Elephant()];
foo(arr);
bar(arr);