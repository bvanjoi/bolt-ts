// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/parenthesizedExpressionInternalComments.ts`, Apache-2.0 License
/*1*/("foo");
/*2*//*2*//*3*//*4*/// open
/*1*/("foo");
// next
/*2*/// next
/*2*///close
/*3*//*4*/(1);
(undefined);
(null);
({});
([]);