// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/nonObjectUnionNestedExcessPropertyCheck.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface IProps {
    iconProp?: string;
    nestedProp?: {
        testBool?: boolean;
    }
}

interface INestedProps {
    nestedProps?: IProps;
}

// These are the types of errors we want:
const propB1: IProps | number = { INVALID_PROP_NAME: 'share', iconProp: 'test' };
//~^ ERROR: Object literal may only specify known properties, and 'INVALID_PROP_NAME' does not exist in type 'IProps'.

// Nested typing works here and we also get an expected error:
const propB2: IProps | number = { nestedProp: { asdfasdf: 'test' }, iconProp: 'test' };
//~^ ERROR: Object literal may only specify known properties, and 'asdfasdf' does not exist in type '{ testBool: undefined | false | true; }'.

// Want an error generated here but there isn't one.
const propA1: INestedProps | number = { nestedProps: { INVALID_PROP_NAME: 'share', iconProp: 'test' } };
//~^ ERROR: Object literal may only specify known properties, and 'INVALID_PROP_NAME' does not exist in type 'IProps'.
