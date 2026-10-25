import {Calculator} from "./file1"
import {test} from "./file1"
//~^ ERROR: 'test' is declared but its value is never read.

var x = new Calculator();
x.handleChar();