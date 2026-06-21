// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/instanceofOnInstantiationExpression.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare class Box<T> {
    value: T;
}


declare const maybeBox: unknown;

maybeBox instanceof Box; // OK

maybeBox instanceof Box<number>; // error
//~^ ERROR: The right-hand side of an 'instanceof' expression must not be an instantiation expression.
maybeBox instanceof (Box<number>); // error
//~^ ERROR: The right-hand side of an 'instanceof' expression must not be an instantiation expression.
maybeBox instanceof ((Box<number>)); // error
//~^ ERROR: The right-hand side of an 'instanceof' expression must not be an instantiation expression.

Box<number> instanceof Object; // OK
(Box<number>) instanceof Object; // OK
((Box<number>)) instanceof Object; // OK
