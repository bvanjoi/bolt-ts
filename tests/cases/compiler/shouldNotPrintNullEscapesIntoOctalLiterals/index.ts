// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/shouldNotPrintNullEscapesIntoOctalLiterals.ts`, Apache-2.0 License

"use strict";
`\x001`;
`\u00001`;
`\u{00000000}1`;
`\u{000000}1`;
`\u{0}1`;