f({
  disc: true,
  cb: (s) => (parseInt(s))  
});
f({
  disc: false,
  cb: (n) => (n.toFixed())  
});
f({
  disc: undefined,
  cb: (n) => (n.toFixed())  
});
f({
  cb: (n) => (n.toFixed())  
});