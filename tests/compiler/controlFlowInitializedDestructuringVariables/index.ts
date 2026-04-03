// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/controlFlowInitializedDestructuringVariables.ts`, Apache-2.0 License

//@compiler-options: strict
//@ run-fail

declare const obj: { a?: string, b?: number };
const {
    a = "0",
    b = +a,
} = obj;


{
    type AbsoluteBigInt<N extends number | bigint> =  `${N}` extends `-${infer Magnitude extends bigint}`
		? Magnitude
		: string;

    function m(a: AbsoluteBigInt<-512n>) {
        const a1: 512n = a;
    }

    0b11_1000n;
}