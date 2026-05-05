// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/templateLiteralIntersection2.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict
//@compiler-options: noEmit

type Path = string & { _pathBrand: any };

type JoinedPath = `${Path}/${Path}`;

declare function joinedPath(p: JoinedPath): void;

joinedPath("foo/bar");
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type '`${Path}/${Path}`'.

declare const somePath: Path;

joinedPath(`${somePath}/${somePath}`);


type StartsWithA = `a${string}`;
type EndsWithA = `${string}a`;


declare function withinAs(p: StartsWithA & EndsWithA): void;

withinAs("");
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type '`a${string}` & `${string}a`'.
withinAs("a");
withinAs("ab");
//~^ ERROR: Argument of type 'string' is not assignable to parameter of type '`a${string}` & `${string}a`'.
withinAs("aba");
withinAs("abavvvva");
