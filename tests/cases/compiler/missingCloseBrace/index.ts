// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/missingCloseBrace.ts`, Apache-2.0 License

function base_init() {
  {

  }

  function me() {

  }
//~ ERROR: '}' expected.