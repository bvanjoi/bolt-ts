// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/assignToPrototype1.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare class Point {
  add(dx: number, dy: number): void;
}

Point.prototype.add = function(dx, dy) {
  const x: string = dx;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
  const y: string = dx;
  //~^ ERROR: Type 'number' is not assignable to type 'string'.
};