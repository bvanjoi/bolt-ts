// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexedAccessRelation.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class Component<S> {
    setState<K extends keyof S>(state: Pick<S, K>) {}
}

export interface State<T> {
    a?: T;
}

class Foo {}

class Comp<T extends Foo, S> extends Component<S & State<T>>
{
    foo(a: T) {
        this.setState({ a: a });
        //~^ ERROR: Type 'T' is not assignable to type 'undefined | S & State<T>["a"]'.
    }
}
