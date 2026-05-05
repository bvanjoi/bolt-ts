// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/lambdaPropSelf.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

declare var ko: any;

class Person {
    children: string[];

    constructor (public name: string, children: string[]) {
        this.children = ko.observableArray(children);
    }

    addChild = () => this.children.push("New child");
}


class T {
     fo() {
        var x = this;
    }
}

namespace M {
    var x = this;
    //~^ ERROR: 'this' cannot be referenced in a module or namespace body.
}
