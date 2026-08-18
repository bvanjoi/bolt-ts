// From `github.com/microsoft/TypeScript/blob/v5.9.3/tests/cases/compiler/arrowFunctionParsingDoesNotConfuseParenthesizedObjectForArrowHead.ts`, Apache-2.0 License


var test = () => (({
  prop: !value,
  run: () => {
    if (!a.b()) {
      return 'special';
    }
    
    return 'default';
  }  
}));