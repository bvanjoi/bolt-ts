// From `github.com/microsoft/TypeScript/blob/v5.7.2/tests/cases/compiler/indexer.ts`, Apache-2.0 License

interface JQueryElement {
  id:string;
}

interface JQuery {
  [n:number]:JQueryElement;
}

var jq:JQuery={ 0: { id : "a" }, 1: { id : "b" } };
jq[0].id;