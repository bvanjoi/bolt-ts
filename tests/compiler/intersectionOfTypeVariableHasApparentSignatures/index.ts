// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/intersectionOfTypeVariableHasApparentSignatures.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: noImplicitAny
//@run-fail

interface Component<P> {
    props: Readonly<P> & Readonly<{ children?: {} }>;
}

interface Props {
    children?: (items: {x: number}) => void
}

declare function f<T extends Props>(i: Component<T>): void;

f({
    props: {
        children: (({ x }) => { })
    }
});