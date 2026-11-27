// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/forwardDeclaredCommonTypes01.ts`, Apache-2.0 License

//@compiler-options: lib=[es5]
//@[target=ES5]     compiler-options: target=es5
//@[target=ES2015]  compiler-options: target=es2015

interface Promise<T> {}
interface Symbol {}
interface Map<K, V> {}
interface WeakMap<K extends object, V> {}
interface Set<T> {}
interface WeakSet<T extends object> {}

(function() {
    new Promise;
    //~^ ERROR: Cannot find name 'Promise'.
    new Symbol; Symbol();
    //~^ ERROR: Cannot find name 'Symbol'.
    //~| ERROR: Cannot find name 'Symbol'.
    new Map;
    //~^ ERROR: Cannot find name 'Map'.
    new WeakMap;
    //~^ ERROR: Cannot find name 'WeakMap'.
    new Set;
    //~^ ERROR: Cannot find name 'Set'.
    new WeakSet;
    //~^ ERROR: Cannot find name 'WeakSet'.
});
