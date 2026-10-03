// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/declFileWithClassNameConflictingWithClassReferredByExtendsClause.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: declaration

declare namespace A.B.Base {
    export class W {
        id: number;
    }
}
namespace X.Y.base {

    export class W extends A.B.Base.W {
        name: string;
        //~^ ERROR: Property 'name' has no initializer and is not definitely assigned in the constructor.
    }
}

namespace X.Y.base.Z {

    export class W<TValue> extends X.Y.base.W {
        value: boolean;
        //~^ ERROR: Property 'value' has no initializer and is not definitely assigned in the constructor.
    }
}