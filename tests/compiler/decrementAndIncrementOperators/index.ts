// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/decrementAndIncrementOperators.ts`, Apache-2.0 License

var x = 0;

// errors
1 ++;
//~^ ERROR: The operand of an increment operator must be a variable or a property

(1)++;
//~^ ERROR: The operand of an increment operator must be a variable or a property
(1)--;
//~^ ERROR: The operand of an decrement operator must be a variable or a property

++(1);
//~^ ERROR: The operand of an increment operator must be a variable or a property
--(1);
//~^ ERROR: The operand of an decrement operator must be a variable or a property

(1 + 2)++;
//~^ ERROR: The operand of an increment operator must be a variable or a property
(1 + 2)--;
//~^ ERROR: The operand of an decrement operator must be a variable or a property

++(1 + 2);
//~^ ERROR: The operand of an increment operator must be a variable or a property
--(1 + 2);
//~^ ERROR: The operand of an decrement operator must be a variable or a property

(x + x)++;
//~^ ERROR: The operand of an increment operator must be a variable or a property
(x + x)--;
//~^ ERROR: The operand of an decrement operator must be a variable or a property

++(x + x);
//~^ ERROR: The operand of an increment operator must be a variable or a property
--(x + x);
//~^ ERROR: The operand of an decrement operator must be a variable or a property

//OK
x++;
x--;

++x;
--x;

(x)++;
--(x);

((x))++;
((x))--;

x[x++]++;
