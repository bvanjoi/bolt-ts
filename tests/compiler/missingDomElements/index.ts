// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/missingDomElements.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[esnext]

interface Element {}
interface EventTarget {}
interface HTMLElement {}
interface HTMLInputElement {}

({} as any as Element).textContent;
//~^ ERROR: Property 'textContent' does not exist on type 'Element'.
({} as any as HTMLElement).textContent;
//~^ ERROR: Property 'textContent' does not exist on type 'HTMLElement'.
({} as any as HTMLInputElement).textContent;
//~^ ERROR: Property 'textContent' does not exist on type 'HTMLInputElement'.
({} as any as EventTarget & HTMLInputElement).textContent
//~^ ERROR: Property 'textContent' does not exist on type 'EventTarget & HTMLInputElement'.

interface HTMLElementFake {}
interface Node {
    actuallyNotTheSame: number;    
};

({} as any as HTMLElementFake).textContent;
//~^ ERROR: Property 'textContent' does not exist on type 'HTMLElementFake'.
({} as any as Node).textContent;
//~^ ERROR: Property 'textContent' does not exist on type 'Node'.
