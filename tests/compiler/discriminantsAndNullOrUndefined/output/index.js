function never(_) {
  throw new Error()
}
function useA(_) {}
function useB(_) {}

if (c !== undefined) {
  switch (c.kind) {
    case 'A':
      useA(c);
      break;
    
    case 'B':
      useB(c);
      break;
    
    default:
      never(c);
    
  }
}
