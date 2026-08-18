// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/coAndContraVariantInferences4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
var SyntaxKind = {};
(function (SyntaxKind) {

  SyntaxKind[SyntaxKind['Modifier'] = 0] = 'Modifier'
  SyntaxKind[SyntaxKind['Decorator'] = 0] = 'Decorator'
})(SyntaxKind);

function foo() {
  every(modifiers, isModifier);
  every(modifiers, isDecorator);
}