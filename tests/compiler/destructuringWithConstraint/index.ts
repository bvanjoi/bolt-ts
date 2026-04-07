// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/destructuringWithConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

interface Props {
    foo?: boolean;
}

function foo<P extends Props>(props: Readonly<P>) {
    let { foo = false } = props;
    if (foo === true) { }
}
