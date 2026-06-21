// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowingTypeofDiscriminant.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=true


function f(wrapped: { value: string } | undefined): string {
  if (typeof wrapped?.value !== 'string') {
      return '42';
  }
  return wrapped.value;
}

function f1(obj: { kind: 'a', data: string } | { kind: 1, data: number }) {
    if (typeof obj.kind === "string") {
        obj;  // { kind: 'a', data: string }
    }
    else {
        obj;  // { kind: 1, data: number }
    }
}

function f2(obj: { kind: 'a', data: string } | { kind: 1, data: number } | undefined) {
    if (typeof obj?.kind === "string") {
        obj;  // { kind: 'a', data: string }
    }
    else {
        obj;  // { kind: 1, data: number } | undefined
    }
}

// Repro from #51700

type WrappedStringOr<T> = { value?: string } | { value?: T };

function numberOk(wrapped: WrappedStringOr<number> | null) {
    if (typeof wrapped?.value !== 'string') {
        return null;
    }
    return wrapped.value;
}

function booleanBad(wrapped: WrappedStringOr<boolean> | null) {
    if (typeof wrapped?.value !== 'string') {
        return null;
    }
    return wrapped.value;
}

function booleanFixed(wrapped: WrappedStringOr<boolean> | null) {
    if (typeof (wrapped?.value) !== 'string') {
        return null;
    }
    return wrapped.value;
}
