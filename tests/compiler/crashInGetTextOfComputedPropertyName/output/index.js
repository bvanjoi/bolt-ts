// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashInGetTextOfComputedPropertyName.ts`, Apache-2.0 License
//@compiler-options: target=es2015
// https://github.com/Microsoft/TypeScript/issues/29006
var itemId = 'some-id'// --- test on first level ---
;
var items = {};
var {[itemId]: itemOk1} = items;
// pass
// --- test on second level ---
typeof itemOk1;
var objWithItems = {
  items: {}  
};
var itemOk2 = objWithItems.items[itemId];
// pass
typeof itemOk2;
var {items: {[itemId]: itemWithTSError} /*happens when default value is provided*/
= {}// in order to re-produce the error, uncomment next line:
} = objWithItems;
typeof itemWithTSError;