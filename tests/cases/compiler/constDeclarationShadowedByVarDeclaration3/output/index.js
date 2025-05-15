// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/constDeclarationShadowedByVarDeclaration3.ts`, Apache-2.0 License
// Ensure only checking for const declarations shadowed by vars
class Rule {
  regex = new RegExp("")
  name = ""
  constructor(name) {this.name = name;}
}