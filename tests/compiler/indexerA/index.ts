// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/indexer3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

class JQueryElement {
    id:string;
    //~^ ERROR: Property 'id' has no initializer and is not definitely assigned in the constructor.
}

class JQuery {
    [n:number]:JQueryElement
}

var jq:JQuery={ 0: { id : "a" }, 1: { id : "b" } };
jq[0].id;