// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/typeInferenceReturnTypeCallback.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface IList<A> {
    map<B>(f: (t: A) => B): IList<B>;
}

class Nil<C> implements IList<C>{
    map<D>(f: (t: C) => D): IList<D> {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'IList<D>'.
    }
}

class Cons<T> implements IList<T>{
    map<U>(f: (t: T) => U): IList<U> {
        return this.foldRight(new Nil<U>(), (t, acc) => {
            return new Cons<U>();
        });
    }

    foldRight<E>(z: E, f: (t: T, acc: E) => E): E {
        return null;
        //~^ ERROR: Type 'null' is not assignable to type 'E'.
    }
}