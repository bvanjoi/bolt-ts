// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/contextualOverloadListFromUnionWithPrimitiveNoImplicitAny.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: strict

// must target esnext for `String.normalize` to exist
type Validate = (text: string, pos: number, self: Rule) => number | boolean;
interface FullRule {
    validate: string | RegExp | Validate;
    normalize?: (match: {x: string}) => void;
}

type Rule = string | FullRule;

const obj: {field: Rule} = {
    field: {
        validate: (_t, _p, _s) => false,
        normalize: match => match.x,
        //~^ ERROR: Parameter 'match' implicitly has an 'any' type.
    }
};