// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericMethodOverspecialization.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@run-fail

var names = ["list", "table1", "table2", "table3", "summary"];

interface HTMLElement {
    clientWidth: number;
    isDisabled: boolean;
}

declare var document: Document;
interface Document {
    getElementById(elementId: string): HTMLElement;
}

const a: HTMLElement = document.getElementById("list");

var elements = names.map(function (name) {
    return document.getElementById(name);
});

var xxx = elements.filter(function (e) {
    return !e.isDisabled;
});

var widths:number[] = elements.map(function (e) { // should not error
    return e.clientWidth;
});
