// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/enumConflictsWithGlobalIdentifier.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: lib=[es5]

enum Position { 
    IgnoreRulesSpecific = 0,
}
var x = IgnoreRulesSpecific.          //~ERROR: Cannot find name 'IgnoreRulesSpecific'. 
var y = Position.IgnoreRulesSpecific; //~ERROR: Identifier expected.
