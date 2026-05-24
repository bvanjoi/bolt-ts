// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/narrowByClauseExpressionInSwitchTrue2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

// https://github.com/microsoft/TypeScript/issues/55986

declare const f: 'a' | 'b' | 'c';

switch(true) {
    case f === 'a':
    case f === 'b':
        f;
        break
    default:
        f;
}

f;

switch(true) {
    case f === 'a':
        f;
    case f === 'b':
        f;
        break
    default:
        f;
}

f;
