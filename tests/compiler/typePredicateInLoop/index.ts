// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/typePredicateInLoop.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface Type {
  type: number;
}

interface TypeExt extends Type {
  arr: Type[];
}

const guard = (arg: Type): arg is TypeExt => arg.type === 1;
const otherFunc = (arg1: Type, arg2: TypeExt): void => {};

export function y(arg: Type): void {
  if (guard(arg)) {
    for (const ITEM of arg.arr) {
      if (otherFunc(ITEM, arg)) {
        //~^ ERROR: An expression of type 'void' cannot be tested for truthiness.
      }
    }
  }
}