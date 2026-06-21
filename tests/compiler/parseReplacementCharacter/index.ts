// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/parseReplacementCharacter.ts`, Apache-2.0 License

//@compiler-options: target=es6

"oops �� oops";
'oops �� oops';
`oops �� oops`;
`${"oops �� oops"}`;
// oops �� oops
/* oops �� oops */
/** oops �� oops */