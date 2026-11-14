// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/erasableSyntaxOnly.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: module=commonjs
//@compiler-options: erasableSyntaxOnly

class MyClassErr {
    // No parameter properties
    constructor(public foo: string) { }
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
}

namespace IllegalBecauseInstantiated {
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
    export const m = 1;
}

namespace AlsoIllegalBecauseInstantiated {
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
    class PrivateClass {

    }
}

namespace IllegalBecauseNestedInstantiated {
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
    namespace Nested {
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
        export const m = 1;
    }
}

enum NotLegalEnum {
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
    B = 1
}

import NoGoodAlias = NotLegalEnum.B;
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.

const enum NotLegalConstEnum {
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
    C = 2
}

// No errors after this point
class MyClassOk {
    // Not a parameter property, ok
    constructor(foo: string) { }
}

// Note for implementors: This should not be an error
// as this entire namespace block is fully erased
namespace NotInstantiated {
    export interface JustAType { }
    export type ATypeInANamespace = {};
    namespace Nested {
        export type ATypeInANamespace = {};
    }
}
declare namespace AmbientIsNotInstantiated {
    export const stillOk = 12;
}

declare enum LegalEnum {
    A = 1
}

declare namespace AmbientStuff {
    namespace Nested {
        export const stillOk = 12;
    }
    enum EnumInAmbientContext {
        B = 1
    }

    import FineAlias = EnumInAmbientContext.B;
}

// Not erasable
(()=><any>{})();
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
(()=>< any >{})();
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
(()=> < any > {})();
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.

// Erasable
(()=><any>({}))();
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
(()=>(<any>{}))();
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
<any>{};
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.


// return and yield ASI
function *gen() {
    yield <any>
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
        1;
    return <any>
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
        1;
}

// at the start of an ExpressionStatement if followed by an object literal; though I'm not sure why one would use it there
<unknown>{foo() {}}.foo();
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.

// at the start of an ExpressionStatement if followed by function keyword
<unknown>function() {}();
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
<unknown>function() {};
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.

// at the start of an ExpressionStatement if followed by an anonymous class expression
// note that this exact syntax currently emits invalid JS (no parenthesis added like for function above)
<unknown>class {}
    //~^ ERROR: This syntax is not allowed when 'erasableSyntaxOnly' is enabled.
