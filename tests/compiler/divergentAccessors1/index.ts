// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/divergentAccessors1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@run-fail

{
    interface IHasGetSet {
        get foo(): number;
        set foo(v: number | string);
    }
    
    const ihgs: IHasGetSet = null as any;
    ihgs.foo = "32";
    let r_ihgs_foo: number = ihgs.foo;
}

{
    type T_HasGetSet = {
        get foo(): number;
        set foo(v: number | string);
    }
    
    const t_hgs: T_HasGetSet = null as any;
    t_hgs.foo = "32";
    let r_t_hgs_foo: number = t_hgs.foo;
}
