// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/emitMethodCalledNew.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

// https://github.com/microsoft/TypeScript/issues/55075

export const a = {
  new(x: number) { return x + 1 }
}
export const b = {
  "new"(x: number) { return x + 1 }
}
export const c = {
  ["new"](x: number) { return x + 1 }
}
