// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/arithAssignTyping.ts`, Apache-2.0 License

class f { }

f += '';   //~ ERROR: Cannot assign to 'f' because it is a class.
f += 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 
f -= 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 
f *= 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 
f /= 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 
f %= 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 
f &= 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 
f |= 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 
f <<= 1;   //~ ERROR: Cannot assign to 'f' because it is a class.  
f >>= 1;   //~ ERROR: Cannot assign to 'f' because it is a class.  
f >>>= 1;  //~ ERROR: Cannot assign to 'f' because it is a class. 
f ^= 1;    //~ ERROR: Cannot assign to 'f' because it is a class. 