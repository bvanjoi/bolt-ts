// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/getAndSetAsMemberNames.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

class C1 {
    set: boolean;
    get = 1;
}
class C2 {
    set;
}
class C3 {
    set (x) {
        return x + 1;
    }
}
class C4 {
    get: boolean = true;
}
class C5 {
    public set: () => boolean = function () { return true; };
    get (): boolean { return true; }
    set t(x) { }
}
