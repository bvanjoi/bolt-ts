// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/multipleClassPropertyModifiers.ts`, Apache-2.0 License

class C {
    public static p1;
    static public p2;
    //~^ ERROR: 'public' modifier must precede 'static' modifier.
    private static p3;
    static private p4;
    //~^ ERROR: 'private' modifier must precede 'static' modifier.
}
