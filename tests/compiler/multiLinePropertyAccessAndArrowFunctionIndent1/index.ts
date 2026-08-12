// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/multiLinePropertyAccessAndArrowFunctionIndent1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

return this.edit(role)
    .then((role: Role) =>
        this.roleService.add(role)
            .then((data: ng.IHttpPromiseCallbackArg<Role>) => data.data));
//~^^^^ ERROR: A 'return' statement can only be used within a function body.
//~| ERROR: Cannot find name 'role'.
//~^^^^^ ERROR: Cannot find name 'Role'.
//~^^^^ ERROR: Cannot find name 'ng'.
//~| ERROR: Cannot find name 'Role'.
