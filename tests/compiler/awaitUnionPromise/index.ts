// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/awaitUnionPromise.ts`, Apache-2.0 License

//@compiler-options: target=es2015

// https://github.com/Microsoft/TypeScript/issues/18186

class AsyncEnumeratorDone { };

interface IAsyncEnumerator<T> {
    next1(): Promise<T | AsyncEnumeratorDone>;
    next2(): Promise<T> | Promise<AsyncEnumeratorDone>;
    next3(): Promise<T | {}>;
    next4(): Promise<T | { x: string }>;
}

async function main() {
    const x: IAsyncEnumerator<number> = null;
    //~^ ERROR: Type 'null' is not assignable to type 'IAsyncEnumerator<number>'. 
    let a = await x.next1();
    let b = await x.next2();
    let c = await x.next3();
    let d = await x.next4();
}
