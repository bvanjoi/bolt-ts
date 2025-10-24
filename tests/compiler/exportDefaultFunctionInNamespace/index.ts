// // From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/exportDefaultFunctionInNamespace.ts`, Apache-2.0 License

namespace ns_function {
    export default function () {}
    //~^ ERROR: A default export can only be used in an ECMAScript-style module.
}

namespace ns_async_function {
    export default async function () {}
    //~^ ERROR: A default export can only be used in an ECMAScript-style module.
}

function f() {
    export default 1;
    //~^ ERROR: A default export must be at the top level of a file or module declaration.
}

namespace m {
    function f() {
            export default 2;
            //~^ ERROR: A default export must be at the top level of a file or module declaration.
    }
}

{
    export default 3;
    //~^ ERROR: A default export must be at the top level of a file or module declaration.
}


{
    export default function () {}
    //~^ ERROR: A default export must be at the top level of a file or module declaration.
}

namespace n {
    export default 4;
    //~^ ERROR: A default export can only be used in an ECMAScript-style module.
}