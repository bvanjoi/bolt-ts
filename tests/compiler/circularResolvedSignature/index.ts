// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularResolvedSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function useState<S>(initialState: (() => S)): [S, (s: S) => void];

type Data = Readonly<{
    value: number;
    foo: (arg: any) => void;
    bar: (arg: any) => void;
}>;

export function Component() {
    const [state, setState] = useState<Data>(() => ({
        value: "string", // this should be a number
        //~^ ERROR: Type 'string' is not assignable to type 'number'.
        foo: (arg) => setState(arg),
        bar: (arg) => setState(arg),
    }));
}