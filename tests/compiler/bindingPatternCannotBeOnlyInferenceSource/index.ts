// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/bindingPatternCannotBeOnlyInferenceSource.ts`, Apache-2.0 License

//@compiler-options: strictNullChecks

declare function f<T>(): T;
const {} = f();       // error (only in strictNullChecks)
//~^ ERROR: Object is of type 'unknown'.
const { p1 } = f();   // error
//~^ ERROR: Property '"p1"' does not exist on type 'unknown'.
const [] = f();       // error
//~^ ERROR: Type 'unknown' is not an array type.
//~| ERROR: Object is of type 'unknown'.
const [e1, e2] = f(); // error
//~^ ERROR: Type 'unknown' is not an array type.
//~| ERROR: Type 'unknown' is not an array type.

// Repro from #43605
type Dispatch<A = { type: any; [extraProps: string]: any }> = { <T extends A>(action: T): T };
type IFuncs = { readonly [key: string]: (...p: any) => void };
type IDestructuring<T extends IFuncs> = { readonly [key in keyof T]?: (...p: Parameters<T[key]>) => void };
type Destructuring<T extends IFuncs, U extends IDestructuring<T>> = (dispatch: Dispatch<any>, funcs: T) => U;
const funcs1 = {
    funcA: (a: boolean): void => {},
    funcB: (b: string, bb: string): void => {},
    funcC: (c: number, cc: number, ccc: boolean): void => {},
};
type TFuncs1 = typeof funcs1;
declare function useReduxDispatch1<T extends IDestructuring<TFuncs1>>(destructuring: Destructuring<TFuncs1, T>): T;
const {} = useReduxDispatch1(
    (d, f) => ({
        funcA: (...p) => d(f.funcA(...p)), // p should be inferrable
        funcB: (...p) => d(f.funcB(...p)),
        funcC: (...p) => d(f.funcC(...p)),
    })
);

{
    function g(a: string, b: string): void {}
    const n = ['hello', 'world'] as const;
    g(...n); // OK
}

{
    type F = (...p1: [boolean]) => void;
    const _a0: F = (p2) => {
        const _: number = p2; //~ ERROR: Type 'boolean' is not assignable to type 'number'.
    };
    const _a1: F = (...p2) => {
        const _: boolean = p2[0]; // p should be inferrable
        p2[1]; //~ ERROR: Tuple type '[boolean]' of length '1' has no element at index '1'.
    };
}

{
    type G0 = (b: string, bb: string) => void;
    type G1 = (...p: Parameters<G0>) => void;
    type G = () => G1;
    const g: G = () => (...p) => {
        let a0: number = p[0];
        //~^ ERROR: Type 'string' is not assignable to type 'number'.
        let a1: number = p[1];
        //~^ ERROR: Type 'string' is not assignable to type 'number'.
        p[2];
        //~^ ERROR: Tuple type '[string, string]' of length '2' has no element at index '2'.
    }
}

{
    function f2(a: () => {funcB: (b: string) => void;}): void {}
    f2(
        () => ({
            funcB: (...p) => {
                let a0: number = p[0];
                //~^ ERROR: Type 'string' is not assignable to type 'number'.
                p[1];
                //~^ ERROR: Tuple type '[string]' of length '1' has no element at index '1'.
            },
        })
    );
}