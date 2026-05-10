// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericReturnTypeFromGetter1.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

export interface A<T> {
   new (dbSet: DbSet<T>): T;
}
export class DbSet<T> {
    _entityType: A;
    //~^ ERROR: Generic type 'A<T>' requires 1 type argument.
  get entityType() { return this._entityType; }  // used to ICE without return type annotation
}
