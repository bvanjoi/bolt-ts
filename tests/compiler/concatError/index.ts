// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/concatError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

var n1: number[];
/*
interface Array<T> {
    concat(...items: T[][]): T[]; // Note: This overload needs to be picked for arrays of arrays, even though both are applicable
    concat(...items: T[]): T[];
}
*/
var fa: number[];

fa = fa.concat([0]);
//~^ ERROR: Variable 'fa' is used before being assigned.
fa = fa.concat(0);





/*




declare class C<T> {
	public m(p1: C<C<T>>): C<T>;
	//public p: T;
}

var c: C<number>;
var cc: C<C<number>>;

c = c.m(cc);
*/