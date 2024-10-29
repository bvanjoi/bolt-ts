// Shouldn't compile (the long form f = f + ""; doesn't):
class f { }

f += ''; //~ ERROR: Cannot assign to 'f' because it is a class.
