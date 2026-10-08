// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashInGetTextOfComputedPropertyName.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// https://github.com/Microsoft/TypeScript/issues/29006
var itemId = 'some-id';
// --- test on first level ---
var items = {};
var {[itemId]: itemOk1} = items;
typeof itemOk1// pass
;// --- test on second level ---

var objWithItems = {
  items: {}  
};
var itemOk2 = objWithItems.items[itemId];
typeof itemOk2// pass
;
var {items: {[itemId]: itemWithTSError} = {}/*happens when default value is provided*/ } = objWithItems;
// in order to re-produce the error, uncomment next line:
typeof itemWithTSError;