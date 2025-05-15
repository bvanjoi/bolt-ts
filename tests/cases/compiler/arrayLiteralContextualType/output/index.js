
// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/arrayLiteralContextualType.ts`, Apache-2.0 License
class Giraffe {
  name = "Giraffe"
  neckLength = "3m"
}
class Elephant {
  name = "Elephant"
  trunkDiameter = "20cm"
}
function foo(animals) {}
function bar(animals) {}
foo([new Giraffe(), new Elephant()]);
// Legal because of the contextual type IAnimal provided by the parameter
bar([new Giraffe(), new Elephant()]);
// Legal because of the contextual type IAnimal provided by the parameter
var arr = [new Giraffe(), new Elephant()];
foo(arr);
// ok because arr is Array<Giraffe|Elephant> not {}[]
bar(arr);