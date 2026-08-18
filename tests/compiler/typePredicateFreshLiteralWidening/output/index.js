// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicateFreshLiteralWidening.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var satisfies = () => ((narrow) => (narrow));
var isNotNull = (value) => (value !== null);
var item1 = satisfies()({
  value: '1'  
});
var item2 = satisfies()({
  value: '2'  
});
var item3 = satisfies()({
  value: null  
});
var values2 = ['1', '2', null];
var filteredValues2 = values2.filter(isNotNull);
var values1 = [item1, item2, item3].map((item) => (item.value));
var filteredValues1 = values1.filter(isNotNull);