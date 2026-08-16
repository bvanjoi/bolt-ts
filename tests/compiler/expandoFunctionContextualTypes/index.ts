// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/expandoFunctionContextualTypes.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface MyComponentProps {
    color: "red" | "blue"
}

interface StatelessComponent<P> {
    (): any;
    defaultProps?: Partial<P>;
}

const MyComponent: StatelessComponent<MyComponentProps> = () => null as any;

MyComponent.defaultProps = {
    color: "red"
};
