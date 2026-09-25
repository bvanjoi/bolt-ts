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