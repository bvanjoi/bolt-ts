declare function Q<T>(value: T): string;
declare namespace Q {
    interface Promise<T> {
        foo: string;
    }
    export function defer<T>(): string;
}