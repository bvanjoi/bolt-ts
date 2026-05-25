// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/complicatedIndexesOfIntersectionsAreInferencable.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

interface FormikConfig<Values> {
    initialValues: Values;
    validate?: (props: Values) => void;
    validateOnChange?: boolean;
}

declare function Func<Values = object, ExtraProps = {}>(
    x: (string extends "validate" | "initialValues" | keyof ExtraProps
        ? Readonly<FormikConfig<Values> & ExtraProps>
        : Pick<Readonly<FormikConfig<Values> & ExtraProps>, "validate" | "initialValues" | Exclude<keyof ExtraProps, "validateOnChange">>
        & Partial<Pick<Readonly<FormikConfig<Values> & ExtraProps>, "validateOnChange" | Extract<keyof ExtraProps, "validateOnChange">>>)
): void;

Func({
    initialValues: {
        foo: ""
    },
    validate: props => {
        props.foo;
    }
});