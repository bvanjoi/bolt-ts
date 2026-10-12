// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/keywordExpressionInternalComments.ts`, Apache-2.0 License

//@compiler-options: target=es2015

/*1*/ new /*2*/ Array /*3*/;
/*1*/ typeof /*2*/ Array /*3*/;
/*1*/ void /*2*/ Array /*3*/;
/*1*/ delete /*2*/ Array.toString /*3*/;
//~^ ERROR: The operand of a 'delete' operator must be optional.