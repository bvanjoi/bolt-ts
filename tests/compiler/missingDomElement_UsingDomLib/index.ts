// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingDomElement_UsingDomLib.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5,dom]

interface HTMLMissingElement {}

(({}) as any as HTMLMissingElement).textContent;
//~^ ERROR: Property 'textContent' does not exist on type 'HTMLMissingElement'.