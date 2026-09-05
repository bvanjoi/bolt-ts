var items = [{
  name: 'A'  
}, {
  name: 'C'  
}, {
  name: 'B'  
}];
for ( var item of items.sort((a, b) => (a.name.localeCompare(b.name)))) {}