var test = {};
(function (test) {

class A {
    foo() {}  
}
test.A = A

class B extends A {
    bar(callback) {}
    runme() {
      this.bar(() => {
        super.foo()      
})    
}  
}
test.B = B

})(test)