class C {
  p1 = 0
  callback(cb) {
    cb()  
}
  doit() {
    this.callback(() => {
      this.p1 + 1    
})  
}
}