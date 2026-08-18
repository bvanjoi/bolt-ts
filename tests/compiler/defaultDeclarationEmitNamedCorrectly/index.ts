// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/defaultDeclarationEmitNamedCorrectly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

export interface Things<P, T> {
    p: P;
    t: T;
}
export function make<P, CTor>(x: { new (): CTor & {props: P} }): Things<P, CTor> {
    return null as any;
}

export interface Props {
}

export default class MyComponent {
    props: Props;
    //~^ ERROR: Property 'props' has no initializer and is not definitely assigned in the constructor.
    static create = make(MyComponent);
}