// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumUsedBeforeDeclaration.ts`, Apache-2.0 License

const v: Color = Color.Green;
//~^ ERROR: Enum 'Color' used before its declaration.
const v2: ConstColor = ConstColor.Green;
enum Color { Red, Green, Blue }
const enum ConstColor { Red, Green, Blue }

