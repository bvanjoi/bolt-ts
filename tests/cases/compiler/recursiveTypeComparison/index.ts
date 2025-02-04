// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/recursiveTypeComparison.ts`, Apache-2.0 License

interface Observable<T> {
  // This member can't be of type T, Property<T>, or Observable<anything but T>
  needThisOne: Observable<T>;
  // Add more to make it slower
  expo1: Property<T[]>; //  0.31 seconds in check
  expo2: Property<T[]>; //  3.11 seconds
  expo3: Property<T[]>; // 82.28 seconds
}
interface Property<T> extends Observable<T> { }

var p: Observable<{}>;
var stuck: Property<number> = p;
