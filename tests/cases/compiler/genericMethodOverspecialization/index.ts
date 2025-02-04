// From `github.com/microsoft/TypeScript/blob/v5.7.3/tests/cases/compiler/genericLambaArgWithoutTypeArguments.ts`, Apache-2.0 License

var names = ["list", "table1", "table2", "table3", "summary"];

interface HTMLElement {
    clientWidth: number;
    isDisabled: boolean;
}

declare var document: Document;
interface Document {
    getElementById(elementId: string): HTMLElement;
}

var elements = names.map(function (name) {
    return document.getElementById(name);
});

var elements2: string[] = names.map(function (name) {
//~^ ERROR: Type 'HTMLElement[]' is not assignable to type 'string[]'.
    return document.getElementById(name);
})

var xxx = elements.filter(function (e) {
    return !e.isDisabled;
});

var widths:number[] = elements.map(function (e) { // should not error
    return e.clientWidth;
});
