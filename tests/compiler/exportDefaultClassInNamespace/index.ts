// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/exportDefaultClassInNamespace.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace ns_class {
    export default class {}
    //~^ ERROR: A default export can only be used in an ECMAScript-style module.
}

namespace ns_abstract_class {
    export default abstract class {}
    //~^ ERROR: A default export can only be used in an ECMAScript-style module.
}
