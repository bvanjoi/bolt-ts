// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/bindingPatternInParameter01.ts`, Apache-2.0 License

const nestedArray = [[[1, 2]], [[3, 4]]];

nestedArray.forEach(([[a, b]]) => {
  console.log(a, b);
});

