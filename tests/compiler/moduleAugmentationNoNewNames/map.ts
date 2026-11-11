import { Observable } from "./observable"

(<any>Observable.prototype).map = function() { }

declare module "./observable" {
    interface Observable<T> {
        map<U>(proj: (e:T) => U): Observable<U>
    }
    class Bar {}
    let y: number, z: string;
    let {a: x, b: x1}: {a: number, b: number};
    namespace Z {}
}