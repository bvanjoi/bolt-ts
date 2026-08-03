// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericMemberFunction.ts`, Apache-2.0 License

//@compiler-options: target=es2020
//@compiler-options: module=esnext

export class BuildError<A, B, C>{
  public parent<A, B extends A, C>(): FileWithErrors<A, B, C> {
    return undefined;
    //~^ ERROR: Type 'undefined' is not assignable to type 'FileWithErrors<A, B, C>'.
  }
}
export class FileWithErrors<A, B, C>{
  public errors<A, B extends A, C>(): BuildError<A, B, C>[] {
    return undefined;
    //~^ ERROR: Type 'undefined' is not assignable to type 'BuildError<A, B, C>[]'.
  }
  public parent<A, B extends A, C>(): BuildResult<A, B, C> {
    return undefined;
    //~^ ERROR: Type 'undefined' is not assignable to type 'BuildResult<A, B, C>'.
  }
}
export class BuildResult<A, B, C>{
  public merge<A, B extends A, C>(other: BuildResult<A, B, C>): void {
    a.b.c.d.e.f.g = 0;
    //~^ ERROR: Cannot find name 'a'.
    removedFiles.forEach(<A, B extends A, C>(each: FileWithErrors<A, B, C>) => {
    //~^ ERROR: Cannot find name 'removedFiles'.
      this.removeFile(each);
    //~^ ERROR: Property 'removeFile' does not exist on type 'BuildResult<A, B, C, BuildResult>'.
    });
  }
}
