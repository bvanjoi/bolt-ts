// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/circularlyReferentialInterfaceAccessNoCrash.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration
//@run-fail

type Mxs = Mx<'list', Mxs['p1']>;
//~^ ERROR: Type arguments for 'Mx' circularly reference themselves.

interface Mx<T, K> {
  p1: T;
  p2: K;
}

type ArrElem = ['list', ArrElem[number][0]][];
//~^ ERROR: Tuple type arguments circularly reference themselves.

type TupleElem = [['list', TupleElem[0][0]]];
//~^ ERROR: Tuple type arguments circularly reference themselves.
