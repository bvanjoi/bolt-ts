// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/twiceNestedKeyofIndexInference.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

type Set1<T, K1 extends keyof T> = T extends any[] ? T : Pick<T, Exclude<keyof T, K1>> & {
    [SK1 in K1]-?: Required<Pick<T, SK1>>;
}[K1];

type Set2<T, K1 extends keyof T, K2 extends keyof T[K1]> = T extends any[] ? T : Pick<T, Exclude<keyof T, K1>> & {
    [SK1 in K1]-?: Required<{
        [key in K1]: Set1<T[K1], K2>;
    }>;
}[K1];

declare function set<T, K1 extends keyof T>(source: T, path: [K1], value: T[K1]): Set1<T, K1>;

declare function set<T, K1 extends keyof T, K2 extends keyof T[K1]>(source: T, path: [K1, K2], value: T[K1][K2]): Set2<T, K1, K2>;


interface State {
    a: {
        b: string;
        c: number;
    };
    d: boolean;
}

const state: State = {
    a: {
        b: "",
        c: 0,
    },
    d: false,
};

const newState: State = set(state, ["a", 'b'], 'why'); // shouldn't be an error