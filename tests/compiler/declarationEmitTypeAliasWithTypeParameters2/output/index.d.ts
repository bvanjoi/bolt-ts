type Bar<X, Y, Z> = () => [X, Y, Z];
type Baz<M, N> = Bar<M, string, N>;
type Baa<Y> = Baz<boolean, Y>;
export var y: (x: Baa<number>) => number;
