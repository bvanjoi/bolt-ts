// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/duplicateIdentifierDifferentSpelling.ts`, Apache-2.0 License


class A {
  0b11 = '';
  3 = '';
  //~^ ERROR: Duplicate identifier '3'.
}

var X = { 0b11: '', 3: '' };
//~^ ERROR: An object literal cannot have multiple properties with the same name.