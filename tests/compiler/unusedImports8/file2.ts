import {Calculator as calc, test as t1, test2 as t2} from "./file1"
//~^ ERROR: 't2' is declared but its value is never read.

var x = new calc();
x.handleChar();
t1();
