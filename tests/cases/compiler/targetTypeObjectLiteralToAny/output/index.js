// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/targetTypeObjectLiteralToAny.ts`, Apache-2.0 License
function suggest() {
  var TypeScriptKeywords;
  var result;
  TypeScriptKeywords.forEach(function (keyword) {
    result.push({text: keyword,
    type: "keyword"});
  });
}