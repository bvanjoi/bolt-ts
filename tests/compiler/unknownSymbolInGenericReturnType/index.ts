// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/unknownSymbolInGenericReturnType.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class Linq {
    public static select<T, S>(values: T[], func: (v: T) => A): any[] { //~ERROR: Cannot find name 'A'
        var result = new Array(values.length);
 
        for (var i = 0; i < values.length; i++) {
            result[i] = func(values[i]);
        }
 
        return result;
    }
}
