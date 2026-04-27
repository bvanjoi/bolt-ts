class A {
  kind = 'A';
}
class B {
  kind = 'B';
}
function f(value) {
  switch (value.kind) {
    case 'A':
      return 0
    
    case 'B':
      return 1
    
  }
}