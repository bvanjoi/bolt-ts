// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/mappedTypePartialNonHomomorphicBaseConstraint.ts`, Apache-2.0 License

export type Errors<D> = { readonly [K in keyof D | "base"]?: string[] };

class Model<D> {
  getErrors(): Errors<D> {
    return { base: ["some base error"] };
  }
}
