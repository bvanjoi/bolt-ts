// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/breakInIterationOrSwitchStatement4.ts`, Apache-2.0 License

for (var i in something) { //~ERROR: Cannot find name 'something'.
  break;
}