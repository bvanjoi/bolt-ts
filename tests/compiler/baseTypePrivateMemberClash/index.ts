// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/baseTypePrivateMemberClash.ts`, Apache-2.0 License

class X {
    private m: number;
}
class Y {
    private m: string;
}

interface Z extends X, Y { }
//~^ ERROR: Interface 'Z' cannot simultaneously extend types 'X' and 'Y'.