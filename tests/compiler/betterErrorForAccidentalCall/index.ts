// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/betterErrorForAccidentalCall.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function foo(): string;

foo()(1 as number).toString();    //~ ERROR: This expression is not callable.

foo()   (1 as number).toString(); //~ ERROR: This expression is not callable.

foo()                             //~ ERROR: This expression is not callable.
(1 as number).toString();

foo()                             //~ ERROR: This expression is not callable.
    (1 + 2).toString();

foo()                             //~ ERROR: This expression is not callable.
    (<number>1).toString();
