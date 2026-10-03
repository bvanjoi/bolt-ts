// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/initializedParameterBeforeNonoptionalNotOptional.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict

export declare function foo({a}?: {
    a?: string;
}): void;
export declare function foo2({a}: {
    a?: string | undefined;
} | undefined, b: string): void;
export declare function foo3({a, b: {c}}: {
    a?: string | undefined;
    b?: {c?: string | undefined;} | undefined;
} | undefined, b: string): void;