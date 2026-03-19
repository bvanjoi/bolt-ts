export function evaluate(expression) {
  if (Array.isArray(expression)) {
    var [operator, ...operands] = expression;
    switch (operator) {
      case 'and':
        {
          return operands.every((child) => (evaluate(child)))
        }
      
      case 'not':
        {
          return !evaluate(operands[0])
        }
      
      default:
        {
          throw new Error(`${operator} is not a supported operator`)
        }
      
    }
  } else {
    return expression === 'true'
  }
  
}