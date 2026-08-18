// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/staticFieldWithInterfaceContext.ts`, Apache-2.0 License
var c = class {
  static x = {
      a: 'a'    
  };
};
c.x = {
  a: 'a'  
};
var ex = 'x';
var c2 = class {
  static [ex] = {
      a: 'a'    
  };
};
c[ex] = {
  a: 'a'  
};
function f(c = class {
  static x = {
      a: 'a'    
  };
}) {}
var {c: c3} = {
  c: class {
    static x = {
          a: 'a'      
    };
  }  
};
var {c: c4 = class {
  static x = {
      a: 'a'    
  };
}} = {};
var {c: c5 = class {
  static x = {
      a: 'a'    
  };
}} = {
  c: class {
    static x = {
          a: 'a'      
    };
  }  
};
var [c6] = [class {
  static x = {
      a: 'a'    
  };
}];
var [c7] = [class {
  static x = {
      a: 'a'    
  };
}];
var [c8 = class {
  static x = {
      a: 'a'    
  };
}] = [];
var [c9 = class {
  static x = {
      a: 'a'    
  };
}] = [];
var [c10 = class {
  static x = {
      a: 'a'    
  };
}] = [class {
  static x = {
      a: 'a'    
  };
}];
var [c11 = class {
  static x = {
      a: 'a'    
  };
}] = [class {
  static x = {
      a: 'a'    
  };
}];