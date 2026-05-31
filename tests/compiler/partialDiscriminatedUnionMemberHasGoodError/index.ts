// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/partialDiscriminatedUnionMemberHasGoodError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface TypeA {
    type: "A";
    param: string;
}

interface TypeB {
    type: "B";
    param: string;
}

type ValidType = TypeA | TypeB;

interface Wrapper {
    types: ValidType[];
}

const foo: Wrapper[] = [];

foo.push({
    types: [{     //~ERROR: Type '{ type: "A"; }' is not assignable to type 'ValidType'.
        type: "A"
    }]
});