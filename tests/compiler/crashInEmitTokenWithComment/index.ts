// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/crashInEmitTokenWithComment.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

// GH#32358
const fn = (param: string) => undefined;

const foo = {bar: 'a'};
fn(({[foo.bar]: c}) => undefined);
//~^ ERROR: Argument of type '({ [computed]: c }: { }) => undefined' is not assignable to parameter of type 'string'.
//~| ERROR: Type '{ }' has no matching index signature for type 'string'.
//~| ERROR: Type '{ }' has no matching index signature for type 'string'.
