// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericConstraint2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

interface Comparable<T> {
    comparer(other: T): number;
}

function compare<T extends Comparable<T>>(x: T, y: T): number {
    if (x == null) return y == null ? 0 : -1;
    if (y == null) return 1;
    return x.comparer(y);
}

class ComparableString implements Comparable<string>{
//~^ ERROR: Property 'comparer' is missing.
    constructor(public currentValue: string) { }

    localeCompare(other) {
        return 0;
    }
}

var a = new ComparableString("a");
var b = new ComparableString("b");
var c = compare<ComparableString>(a, b);
//~^ ERROR: Property 'comparer' is missing.