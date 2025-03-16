// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/maxConstraints.ts`, Apache-2.0 License

interface Comparable<T> {
  compareTo(other: T): number;
}
interface Comparer {
  <T extends Comparable<T>>(x: T, y: T): T;
}
var max2: Comparer = (x, y) => { return (x.compareTo(y) > 0) ? x : y };
var maxResult = max2(1, 2);
//~^ ERROR: Argument of type 'number' is not assignable to parameter of type 'Comparable<1 | 2>'.
//~| ERROR: Argument of type 'number' is not assignable to parameter of type 'Comparable<1 | 2>'.
