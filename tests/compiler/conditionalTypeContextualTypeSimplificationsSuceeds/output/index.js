// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/conditionalTypeContextualTypeSimplificationsSuceeds.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function bad(attrs) {}
function good1(attrs) {}
function good2(attrs) {}
bad({
  when: (value) => (false)  
});
good1({
  when: (value) => (false)  
});
good2({
  when: (value) => (false)  
});