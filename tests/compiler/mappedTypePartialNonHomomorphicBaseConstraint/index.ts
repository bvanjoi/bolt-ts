// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/mappedTypePartialNonHomomorphicBaseConstraint.ts`, Apache-2.0 License

//@compiler-options: target=es2015

export type Errors<D> = { readonly [K in keyof D | "base"]?: string[] };

class Model<D> {
  getErrors(): Errors<D> {
    return { base: ["some base error"] };
  }
}
