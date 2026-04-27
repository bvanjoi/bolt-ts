// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/overloadedConstructorFixesInferencesAppropriately.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Box<T> {
    v: T;
}

interface ErrorResult {
    readonly error: true
}

interface AsyncLoaderProps<TResult extends {}> {
    readonly asyncLoad: () => Box<TResult>;
    readonly children: (result: Exclude<TResult, ErrorResult>) => string;
}

class AsyncLoader<TResult extends {}> {
    constructor(props: string, context: any);
    constructor(props: AsyncLoaderProps<TResult>);
    constructor(...args: any[]) {}
}

function load(): Box<{ success: true } | ErrorResult> {
    return null as any;
}

new AsyncLoader({
    asyncLoad: load,
    children: result => result.success as any,
}); // should work fine
