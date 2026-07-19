// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/baseTypePrivateMemberClash.ts`, Apache-2.0 License

class X {
    private m: number;
    //~^ ERROR: Property 'm' has no initializer and is not definitely assigned in the constructor.
}
class Y {
    private m: string;
    //~^ ERROR: Property 'm' has no initializer and is not definitely assigned in the constructor.
}

interface Z extends X, Y { }
//~^ ERROR: Interface 'Z' cannot simultaneously extend types 'X' and 'Y'.
