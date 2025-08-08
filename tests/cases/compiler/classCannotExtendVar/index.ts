// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/classCannotExtendVar.ts`, Apache-2.0 License

var Markup;

class Markup {      //~ ERROR: Duplicate identifier 'Markup'.
    constructor() {
    }
}

