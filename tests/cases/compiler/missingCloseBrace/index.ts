// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/missingCloseBrace.ts`, Apache-2.0 License

function base_init() {
  {

  }

  function me() {

  }
//~ ERROR: '}' expected.