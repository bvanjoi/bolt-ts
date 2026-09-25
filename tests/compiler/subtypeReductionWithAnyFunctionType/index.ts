// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/subtypeReductionWithAnyFunctionType.ts`, Apache-2.0 License

//@compiler-options: strict
//@compiler-options: target=esnext
//@compiler-options: noEmit

// https://github.com/microsoft/typescript-go/issues/849

declare function useMemo<T>(func: () => T): T;

function getPredicate(alwaysTrue: boolean) {
    const predicate: (input: string) => boolean = useMemo(() => {
        if (alwaysTrue) {
            return () => true;
        }
        return x => x.length > 0;
        //~^ ERROR: Parameter 'x' implicitly has an 'any' type.
    });
    return predicate;
}

// https://github.com/microsoft/typescript-go/issues/1016

declare function compact<T>(array: T[]): T[];
declare function makeFooer(): Fooer;
interface Fooer {
    foo: (v: string) => string;
}
function f() {
    const _ = compact([makeFooer(), { foo: (v) => v }]);
}
