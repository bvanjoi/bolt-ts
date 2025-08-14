// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/class2.ts`, Apache-2.0 License

class foo { constructor() { static f = 3; } }
//~^ ERROR: Declaration or statement expected.
//~| ERROR: Cannot find name 'f'.
