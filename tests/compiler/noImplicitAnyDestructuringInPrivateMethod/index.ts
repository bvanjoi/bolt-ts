// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitAnyDestructuringInPrivateMethod.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: noImplicitAny
//@compiler-options: declaration

type Arg = {
    a: number;
};
export class Bar {
    private bar({ a, }: Arg): number {
        return a;
    }
}
export declare class Bar2 {
    private bar({ a, });
}
