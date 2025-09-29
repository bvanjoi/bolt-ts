// From `github.com/microsoft/TypeScript/blob/v5.9.2/tests/cases/compiler/enumMemberResolution.ts`, Apache-2.0 License

enum Position2 {
    IgnoreRulesSpecific = 0
}
var x = IgnoreRulesSpecific. // error
//~^ ERROR: Cannot find name 'IgnoreRulesSpecific'.
var y = 1;
//~^ ERROR: Identifier expected.
var z = Position2.IgnoreRulesSpecific; // no error
