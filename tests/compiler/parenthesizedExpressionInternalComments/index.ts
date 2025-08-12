// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/parenthesizedExpressionInternalComments.ts`, Apache-2.0 License

/*1*/(/*2*/ "foo" /*3*/)/*4*/
;

// open
/*1*/(
    // next
    /*2*/"foo"
    //close
    /*3*/)/*4*/
;
(1);
(undefined);
(null);
({});
([]);