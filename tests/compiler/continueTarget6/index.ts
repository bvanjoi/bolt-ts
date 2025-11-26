// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/continueTarget6.ts`, Apache-2.0 License

while (true) {
  continue target;
  //~^ ERROR: Jump target cannot cross function boundary.
}