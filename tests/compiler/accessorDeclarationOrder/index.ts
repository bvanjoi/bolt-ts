// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/accessorDeclarationOrder.ts`, Apache-2.0 License

//@compiler-options: target=esnext

class C1 {
    #name: string;
    //~^ ERROR: Property '#name' has no initializer and is not definitely assigned in the constructor.

    public get name() {
        return this.#name;
    }

    private set name(name: string) {
        this.#name = name;
    }
}

class C2 {
    #name: string;
    //~^ ERROR: Property '#name' has no initializer and is not definitely assigned in the constructor.

    private set name(name: string) {
        this.#name = name;
    }

    public get name() {
        return this.#name;
    }
}

const c1 = new C1();
const c2 = new C2();


// no error
c1.name;

// no error
c2.name;

function f0({ bar } = { bar: 0}) {
    //~^ ERROR: This parameter is not allowed with 'use strict' directive.
    "use strict";
}

class C3 {
    constructor({ bar } = { bar: 0}) {
    //~^ ERROR: This parameter is not allowed with 'use strict' directive.
        "use strict";
    }

    f({ bar } = { bar: 0}) {
    //~^ ERROR: This parameter is not allowed with 'use strict' directive.
        'use strict'
    }

    get a({ bar } = { bar: 0}) {
    //~^ ERROR: A 'get' accessor cannot have parameters.
        'use strict'
        return 123;
    }
}

const f1 = function({ bar } = { bar: 0}) {
    //~^ ERROR: This parameter is not allowed with 'use strict' directive.
    "use strict";
}

const f2 = {
    f({ bar } = { bar: 0}) {
    //~^ ERROR: This parameter is not allowed with 'use strict' directive.
        'use strict'
    }
}