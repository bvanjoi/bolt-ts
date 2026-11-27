import {Calculator, test, test2} from "./file1"
//~^ ERROR: 'test' is declared but its value is never read.

var x = new Calculator();
x.handleChar();
test2();