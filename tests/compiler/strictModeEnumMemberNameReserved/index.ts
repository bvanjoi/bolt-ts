// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/strictModeEnumMemberNameReserved.ts`, Apache-2.0 License

"use strict";
enum E {
    static
}

const x1: E.static = E.static;
