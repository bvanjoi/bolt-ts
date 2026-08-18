// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/discriminantUsingEvaluatableTemplateExpression.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strictNullChecks
function never(_) {
  throw new Error()
}
function useA(_) {}
function useB(_) {}

if (c !== undefined) {
  switch (c.kind) {
    case 'A':
      useA(c);
      break;
    
    case 'B':
      useB(c);
      break;
    
    default:
      never(c);
    
  }
}
