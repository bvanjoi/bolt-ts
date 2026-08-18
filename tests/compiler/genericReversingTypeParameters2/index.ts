// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericReversingTypeParameters2.ts`, Apache-2.0 License

//@compiler-options: target=es2015


class BiMap<K, V> {
    private inverseBiMap: BiMap<V, K>;
    //~^ ERROR: Property 'inverseBiMap' has no initializer and is not definitely assigned in the constructor.
    public get(key: K): V { return null; }
    //~^ ERROR: Type 'null' is not assignable to type 'V'.
    public inverse(): BiMap<V, K> { return null; }
    //~^ ERROR: Type 'null' is not assignable to type 'BiMap<V, K>'.
}

var b = new BiMap<string, number>();
var i = b.inverse(); // used to get the type wrong here.
var r2b = i.get(1); 