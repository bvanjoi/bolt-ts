// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/controlFlowFavorAssertedTypeThroughTypePredicate.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

declare function isObject1(value: unknown): value is Record<string, unknown>;

declare const obj1: {};
if (isObject1(obj1)) {
    obj1;
    obj1['attr'];
}
// check type after conditional block
obj1;

declare const obj2: {} | undefined;
if (isObject1(obj2)) {
    obj2;
    obj2['attr'];
}
// check type after conditional block
obj2;

declare function isObject2(value: unknown): value is {};

declare const obj3: Record<string, unknown>;
if (isObject2(obj3)) {
    obj3;
    obj3['attr'];
}
// check type after conditional block
obj3;

declare const obj4: Record<string, unknown> | undefined;
if (isObject2(obj4)) {
    obj4;
    obj4['attr'];
}
// check type after conditional block
obj4;
