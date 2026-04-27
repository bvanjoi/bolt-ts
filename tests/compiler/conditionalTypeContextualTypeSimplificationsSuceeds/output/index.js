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