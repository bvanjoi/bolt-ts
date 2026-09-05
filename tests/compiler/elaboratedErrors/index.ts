// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/elaboratedErrors.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs

interface FileSystem {
  read: number;
}

function fn(s: WorkerFS): void;
function fn(s: FileSystem): void;
function fn(s: FileSystem|WorkerFS) { }

// This should issue a large error, not a small one
class WorkerFS implements FileSystem {
  read: string;
  //~^ ERROR: Property 'read' in type 'WorkerFS<WorkerFS>' is not assignable to the same property in base type 'FileSystem'.
  //~| ERROR: Property 'read' has no initializer and is not definitely assigned in the constructor.
}

interface Alpha { x: string; }
interface Beta { y: number; }
declare var x: Alpha;
declare var y: Beta;

// Only one of these errors should be large
x = y;
//~^ ERROR: Property 'x' is missing.
x = y;
//~^ ERROR: Property 'x' is missing.

// Only one of these errors should be large
y = x;
//~^ ERROR: Property 'y' is missing.
y = x;
//~^ ERROR: Property 'y' is missing.
