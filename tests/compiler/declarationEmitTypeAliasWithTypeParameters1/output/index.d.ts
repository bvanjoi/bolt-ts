type Bar<X, Y> = () => [X, Y];
type Foo<Y> = Bar<any, Y>;
export var y: (x: Foo<string>) => number;
