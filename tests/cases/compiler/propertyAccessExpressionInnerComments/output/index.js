// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/propertyAccessExpressionInnerComments.ts`, Apache-2.0 License
/*1*/Array.toString;
/*2*//*3*//*4*//*1*/Array.toString;
/*2*//*3*/// Single-line comment
/*4*//*1*//*4*//*1*/Array.toString;
/*2*//*3*/// Single-line comment
/*4*//*1*//*4*//*1*/Array.toString;
// Single-line comment
/*2*//*3*//*4*//* Existing issue: the "2" comments below are duplicated and "3"s are missing *//*1*/Array.toString;
/*2*//*3*//*3*//*4*//*1*/Array.toString;
/*2*//*3*/// Single-line comment
/*3*/// Single-line comment
/*4*//*1*//*4*//*1*/Array.toString;
/*2*//*3*/// Single-line comment
/*3*/// Single-line comment
/*4*//*1*//*4*//*1*/Array.toString;