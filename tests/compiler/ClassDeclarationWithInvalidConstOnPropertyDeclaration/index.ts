// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/ClassDeclarationWithInvalidConstOnPropertyDeclaration.ts`, Apache-2.0 License

class AtomicNumbers {
  static const H = 1;
  //~^ ERROR: A class member cannot have the 'const' keyword.
}

class C {
  static H = 1;
}
