var v = 4 | null;
//~^ ERROR: The value 'null' cannot be used here.

(v >> 2) | (v >>> 3);
(v << 2) | (v >>> 3);
