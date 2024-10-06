var v = a 
  ? b
    ? d
    : e
  : c
    ? f
    : g;
//~^^^^^^^ ERROR: Cannot find name 'a'.
//~^^^^^^^ ERROR: Cannot find name 'b'.
//~^^^^^^^ ERROR: Cannot find name 'd'.
//~^^^^^^^ ERROR: Cannot find name 'e'.
//~^^^^^^^ ERROR: Cannot find name 'c'. 
//~^^^^^^^ ERROR: Cannot find name 'f'.
//~^^^^^^^ ERROR: Cannot find name 'g'.