// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/convertKeywords.ts`, Apache-2.0 License

//@compiler-options: module=commonjs

var nake;
function doCompile<P0, P1, P2>(fileset: P0, moduleType: P1) {

    return undefined;
}
export var compileServer = task<number, number, any>(<P0, P1, P2>() => {
  //~^ ERROR: Cannot find name 'task'.

    var folder = path.join(),
        //~^ ERROR: Cannot find name 'path'.
        fileset = nake.fileSetSync<number, number, any>(folder)
        //~^ ERROR: Untyped function calls may not accept type arguments.
  return doCompile<number, number, any>(fileset, moduleType);
  //~^ ERROR: Cannot find name 'moduleType'.
});

