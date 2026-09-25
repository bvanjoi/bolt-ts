var SyntaxKind = {};
(function (SyntaxKind) {

  SyntaxKind[SyntaxKind['Modifier'] = 0] = 'Modifier'
  SyntaxKind[SyntaxKind['Decorator'] = 0] = 'Decorator'
})(SyntaxKind);

function foo() {
  every(modifiers, isModifier);
  every(modifiers, isDecorator);
}