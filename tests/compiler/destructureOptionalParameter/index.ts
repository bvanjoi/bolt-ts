// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/destructureOptionalParameter.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
//@compiler-options: declaration

declare function f1({ a, b }?: { a: number, b: string }): void;

function f2({ a, b }: { a: number, b: number } = { a: 0, b: 0 }) {
    a;
    b;
}

// Repro from #8681

interface Type { t: void }
interface QueryMetadata { q: void }

interface QueryMetadataFactory {
    (selector: Type | string, {descendants, read}?: {
        descendants?: boolean;
        read?: any;
    }): ParameterDecorator;
    new (selector: Type | string, {descendants, read}?: {
        descendants?: boolean;
        read?: any;
    }): QueryMetadata;
}
