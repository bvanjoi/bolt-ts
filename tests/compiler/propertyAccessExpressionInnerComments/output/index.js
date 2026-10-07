// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/propertyAccessExpressionInnerComments.ts`, Apache-2.0 License
/*1*/ Array./*2*/ toString/*3*/ /*1*/ ;/*4*/ 
/*2*/ Array// Single-line comment
./*3*/ /*1*/ toString/*4*/ ;/*2*/ 
// Single-line comment
Array/*3*/ /*1*/ ./*4*/ // Single-line comment
/*2*/ toString;/*3*/ 
/* Existing issue: the "2" comments below are duplicated and "3"s are missing */
/*1*/ Array/*4*/ ./*2*/ toString/*3*/ /*1*/ ;/*4*/ 
/*2*/ Array// Single-line comment
./*3*/ /*1*/ toString/*4*/ ;/*2*/ 
// Single-line comment
Array/*3*/ /*1*/ ./*4*/ // Single-line comment
/*2*/ toString;/*3*/ 
Array.toString;
Array.toString;