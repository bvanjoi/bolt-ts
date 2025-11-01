// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/missingCommaInTemplateStringsArray.ts`, Apache-2.0 License

var array = [
    `template string 1` //~ERROR: It is likely that you are missing a comma to separate these two template expressions. They form a tagged template expression which cannot be invoked.
    `template string 2`
  ];