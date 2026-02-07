// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminatedUnionWithIndexSignature.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

export interface UnionAltA {
    type?: 'text';
}

export interface UnionAltB {
    type?: 'image' | 'video' | 'document';
}

export type ValueUnion = UnionAltA | UnionAltB;

export type MapOrSingleton =
    | {
        [key: string]: ValueUnion;
    }
    | ValueUnion;

const withoutAsConst: MapOrSingleton = {
    1: {
        type: 'text' /*as const*/,
    },
};

const withAsConst: MapOrSingleton = {
    1: {
        type: 'text' as const,
    },
};