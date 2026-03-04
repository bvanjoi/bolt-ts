// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/staticMustPrecedePublic.ts`, Apache-2.0 License

//@compiler-options: target=es2015
class Outer {
    static public intI: number;
    //~^ ERROR: 'public' modifier must precede 'static' modifier.
    static private stringF: string;
    //~^ ERROR: 'private' modifier must precede 'static' modifier.
    public static boolB: boolean;
    private static strNumS: string | number;
}
