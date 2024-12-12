var a = true;
var b = 1;
a ^= a;     //~ERROR: The '^=' operator is not allowed for boolean types. Consider using '!==' instead.
a = true;
b ^= b;
b = 1;
a ^= b;     //~ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
a = true;
b ^= a;     //~ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.  
b = 1;

var c = false;
var d = 2;
c &= c;     //~ERROR: The '&=' operator is not allowed for boolean types. Consider using '&&' instead. 
c = false;
d &= d;
d = 2;
c &= d;     //~ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
c = false;
d &= c;     //~ERROR: The right-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.

var e = true;
var f = 0;
e |= e;     //~ERROR: The '|=' operator is not allowed for boolean types. Consider using '||' instead. 
e = true;
f |= f;
f = 0;
e |= f;     //~ERROR: The left-hand side of an arithmetic operation must be of type 'any', 'number', 'bigint' or an enum type.
e = true;
f |= f;

