var DEFAULT = 'A';
class C {
  D = DEFAULT;
  method() {
    switch (this.D) {
      case 'A':
        break;
      
      case 'B':
        break;
      
    }
  }
}

expectAB(c.D);
c.D = 'B';
class D {
  static SD = DEFAULT;
}
D.SD = 'B';