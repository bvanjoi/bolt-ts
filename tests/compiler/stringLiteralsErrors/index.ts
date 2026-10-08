// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/stringLiteralsErrors.ts`, Apache-2.0 License

// @target: ES2015

// Srtings missing line terminator
var es1 = "line 1     //~ ERROR: Unterminated string literal.
";                    //~ ERROR: Unterminated string literal.
                      //~| ERROR: Expected ','.
var es2 = 'line 1     //~ ERROR: Unterminated string literal.
';                    //~ ERROR: Unterminated string literal.
                      //~| ERROR: Expected ','.
// Space after backslash
var es3 = 'line 1\    //~ ERROR: Unterminated string literal.
';                    //~ ERROR: Unterminated string literal.
                      //~| ERROR: Expected ','.
var es4 = 'line 1\    //~ ERROR: Unterminated string literal.
';                    //~ ERROR: Unterminated string literal.
                      //~| ERROR: Expected ','.

// Unterminated strings
var es5 = "unterminated     //~ERROR: Unterminated string literal.
var es6 = 'unterminated     //~ERROR: Unterminated string literal.
                            //~| ERROR: Expected ','.

// wrong terminator
var es7 = "unterminated '   //~ERROR: Unterminated string literal.
var es8 = 'unterminated "   //~ERROR: Unterminated string literal.
                            //~| ERROR: Expected ','.

// wrong unicode sequences
var es9 = "\u00";           //~ERROR: Hexadecimal digit expected.
var es10 = "\u00GG";        //~ERROR: Hexadecimal digit expected.
var es11 = "\x0";           //~ERROR: Hexadecimal digit expected.
var es12 = "\xmm";          //~ERROR: Hexadecimal digit expected.

// End of file
var es13 = " 
//~^ ERROR: Unterminated string literal.