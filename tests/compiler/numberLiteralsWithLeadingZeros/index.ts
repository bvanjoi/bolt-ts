// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/numberLiteralsWithLeadingZeros.ts`, Apache-2.0 License

00;
//~^ ERROR: Octal literals are not allowed.
000;
//~^ ERROR: Octal literals are not allowed.
01;
//~^ ERROR: Octal literals are not allowed.
001;
//~^ ERROR: Octal literals are not allowed.
08;
//~^ ERROR: Decimals with leading zeros are not allowed.
008;
//~^ ERROR: Decimals with leading zeros are not allowed.

00.5;
//~^ ERROR: Octal literals are not allowed.
000.5;
//~^ ERROR: Octal literals are not allowed.
01.5;
//~^ ERROR: Octal literals are not allowed.
001.5;
//~^ ERROR: Octal literals are not allowed.
08.5;
//~^ ERROR: Decimals with leading zeros are not allowed.
008.5;
//~^ ERROR: Decimals with leading zeros are not allowed.

00e5;
//~^ ERROR: Octal literals are not allowed.
000e5;
//~^ ERROR: Octal literals are not allowed.
01e5;
//~^ ERROR: Octal literals are not allowed.
001e5;
//~^ ERROR: Octal literals are not allowed.
08e5;
//~^ ERROR: Decimals with leading zeros are not allowed.
008e5;
//~^ ERROR: Decimals with leading zeros are not allowed.

00.5e5;
//~^ ERROR: Octal literals are not allowed.
000.5e5;
//~^ ERROR: Octal literals are not allowed.
01.5e5;
//~^ ERROR: Octal literals are not allowed.
001.5e5;
//~^ ERROR: Octal literals are not allowed.
08.5e5;
//~^ ERROR: Decimals with leading zeros are not allowed.
008.5e5;
//~^ ERROR: Decimals with leading zeros are not allowed.

00.5_5;
//~^ ERROR: Octal literals are not allowed.
000.5_5;
//~^ ERROR: Octal literals are not allowed.
01.5_5;
//~^ ERROR: Octal literals are not allowed.
001.5_5;
//~^ ERROR: Octal literals are not allowed.
08.5_5;
//~^ ERROR: Decimals with leading zeros are not allowed.
008.5_5;
//~^ ERROR: Decimals with leading zeros are not allowed.

00e5_5;
//~^ ERROR: Octal literals are not allowed.
000e5_5;
//~^ ERROR: Octal literals are not allowed.
01e5_5;
//~^ ERROR: Octal literals are not allowed.
001e5_5;
//~^ ERROR: Octal literals are not allowed.
08e5_5;
//~^ ERROR: Decimals with leading zeros are not allowed.
008e5_5;
//~^ ERROR: Decimals with leading zeros are not allowed.

00.5_5e5_5;
//~^ ERROR: Octal literals are not allowed.
000.5_5e5_5;
//~^ ERROR: Octal literals are not allowed.
01.5_5e5_5;
//~^ ERROR: Octal literals are not allowed.
001.5_5e5_5;
//~^ ERROR: Octal literals are not allowed.
08.5_5e5_5;
//~^ ERROR: Decimals with leading zeros are not allowed.
008.5_5e5_5;
//~^ ERROR: Decimals with leading zeros are not allowed.

0_0.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_00.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_0.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_1.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_01.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_1.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_8.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_08.5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_8.5_5;
//~^ ERROR: Numeric separators are not allowed here.

0_0e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_00e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_0e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_1e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_01e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_1e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_8e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_08e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_8e5_5;
//~^ ERROR: Numeric separators are not allowed here.

0_0.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_00.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_0.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_1.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_01.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_1.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_8.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_08.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.
0_0_8.5_5e5_5;
//~^ ERROR: Numeric separators are not allowed here.

09.x
//~^ ERROR: Decimals with leading zeros are not allowed.
//~| ERROR: Cannot find name 'x'.