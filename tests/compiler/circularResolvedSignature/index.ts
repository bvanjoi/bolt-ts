// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/circularResolvedSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015

declare function useState<S>(initialState: (() => S)): [S, (s: S) => void];

type Data = Readonly<{
    value: number;
    foo: (arg: any) => void;
    bar: (arg: any) => void;
}>;

export function Component() {
    const [state, setState] = useState<Data>(() => ({   //~ERROR: Type '{ value: string; foo: (arg: any) => void; bar: (arg: any) => void; }' is not assignable to type 'Readonly<{ value: number; foo: (arg: any) => void; bar: (arg: any) => void; }>'.
        value: "string", // this should be a number
        foo: (arg) => setState(arg),
        bar: (arg) => setState(arg),
    }));
}