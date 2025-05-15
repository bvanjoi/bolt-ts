

// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/recursiveTypeComparison.ts`, Apache-2.0 License
// This member can't be of type T, Property<T>, or Observable<anything but T>
// Add more to make it slower
//  0.31 seconds in check
//  3.11 seconds
// 82.28 seconds
var p;
var stuck = p;