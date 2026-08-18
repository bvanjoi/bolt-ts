// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/customAsyncIterator.ts`, Apache-2.0 License
//@compiler-options: target=esnext
//@compiler-options: useDefineForClassFields
class ConstantIterator {
  constructor(constant) {}
  next(value) {
    if (value != null) {
      throw new Error('ConstantIterator.prototype.next may not take any values')
    }
    
    return {
          value: this.constant,
      done: false      
    };
  }
}