

// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/checkInfiniteExpansionTermination2.ts`, Apache-2.0 License


function fn() {
  var values = [];
  // Hang when using <T>, but not <any>
  combineLatest(values);
}