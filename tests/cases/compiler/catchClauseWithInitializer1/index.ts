// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/catchClauseWithInitializer1.ts`, Apache-2.0 License

try {
}
catch (e = 1) { //~ ERROR: Catch clause variable cannot have an initializer.
}