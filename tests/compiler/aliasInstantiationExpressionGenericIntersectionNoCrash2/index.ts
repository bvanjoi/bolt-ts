// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/aliasInstantiationExpressionGenericIntersectionNoCrash2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

declare class Class<T> {
  x: T;
}

declare function fn<T>(): T;


type ClassAlias<T> = typeof Class<T>;
type FnAlias<T> = typeof fn<T>;

type Wat<T> = ClassAlias<T> & FnAlias<T>;


declare const wat: Wat<number>;
wat as Wat<string>;
//~^ ERROR: Conversion of type 'Wat' to type 'Wat' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.