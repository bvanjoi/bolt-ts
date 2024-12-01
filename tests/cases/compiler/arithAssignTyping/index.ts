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