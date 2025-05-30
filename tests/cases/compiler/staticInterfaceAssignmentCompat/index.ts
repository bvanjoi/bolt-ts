// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/staticInterfaceAssignmentCompat.ts`, Apache-2.0 License

class Shape {
  static create(): Shape {
      return new Shape();
  }
}

interface ShapeFactory {
  create(): Shape;
}

var x: ShapeFactory = Shape;
