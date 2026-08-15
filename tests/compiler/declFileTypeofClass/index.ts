// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileTypeofClass.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

class c {
    static x : string;
    private static y: number;
    private x3: string;
    //~^ ERROR: Property 'x3' has no initializer and is not definitely assigned in the constructor.
    public y3: number;
    //~^ ERROR: Property 'y3' has no initializer and is not definitely assigned in the constructor.
}

var x: c;
var y = c;
var z: typeof c;
class genericC<T>
{
}
var genericX = genericC;
