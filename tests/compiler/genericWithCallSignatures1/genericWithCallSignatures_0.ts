interface Callable<T> {
    (): T;
    (value: T): void;
}

interface CallableExtention<T> extends Callable<T> { }