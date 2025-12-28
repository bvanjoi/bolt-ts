// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/aliasInstantiationExpressionGenericIntersectionNoCrash1.ts`, Apache-2.0 License

//@compiler-options: strict

class ErrImpl<E> {
  e!: E;
}

declare const Err: typeof ErrImpl & (<T>() => T);

type ErrAlias<U> = typeof Err<U>;

declare const e: ErrAlias<number>;
e as ErrAlias<string>;
//~^ ERROR: Conversion of type 'new () => ErrImpl<number> & () => number' to type 'new () => ErrImpl<string> & () => string' may be a mistake because neither type sufficiently overlaps with the other. If this was intentional, convert the expression to 'unknown' first.

null! as string;

[] as ['b'?];