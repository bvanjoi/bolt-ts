class A {
  withinObjectLiteralGetterName = {
      get [this.withinObjectLiteralGetterName]() {
      return true;
    }    
  };
}