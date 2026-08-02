// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/unionExcessPropsWithPartialMember.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

interface A {
    unused?: string;
    x: string;
}

interface B {
    x: string;
    y: string;
}

declare var ab: A | B;
declare var a: A;

ab = {...a, y: (null as any as string | undefined)}; // Should be allowed, since `y` is missing on `A`
