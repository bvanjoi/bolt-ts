var itemId = 'some-id';
var items = {};
var {[itemId]: itemOk1} = items;
typeof itemOk1;
var objWithItems = {
  items: {}  
};
var itemOk2 = objWithItems.items[itemId];
typeof itemOk2;
var {items: {[itemId]: itemWithTSError} = {}} = objWithItems;
typeof itemWithTSError;