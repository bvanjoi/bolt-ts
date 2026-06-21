// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declarationEmitMergedAliasWithConst.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: declaration

export const Color = {
    Red: "Red",
    Green: "Green",
    Blue: "Blue"
} as const

export type Color = typeof Color
export type Colors = Color[keyof Color]