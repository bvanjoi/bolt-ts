export declare class Observable<T> {
    filter(pred: (e:T) => boolean): Observable<T>;
}

export namespace Observable {
    export let someValue: number;
}