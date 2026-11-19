declare namespace X { export interface foo { } }
export { X };
export declare function foo(): X.foo;
export declare function bar(): X.bar; // error
//~^ ERROR: Namespace 'X' has no exported member 'bar'.