var example = {
  get foo() {
    return (item) => (this.bar(item));
  }  
};