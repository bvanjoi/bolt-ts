// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/customAsyncIterator.ts`, Apache-2.0 License

//@compiler-options: target=esnext
//@compiler-options: useDefineForClassFields

// GH: https://github.com/microsoft/TypeScript/issues/33239
class ConstantIterator<T> implements AsyncIterator<T, void, T | undefined> {
    constructor(private constant: T) {
    }
    async next(value?: T): Promise<IteratorResult<T>> {
        if (value != null) {
            throw new Error("ConstantIterator.prototype.next may not take any values");
        }
        return { value: this.constant, done: false };
    }
}