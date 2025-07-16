// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/argumentsUsedInClassFieldInitializerOrStaticInitializationBlock.ts`, Apache-2.0 License

function A() {
  return class T {
     a = arguments //~ERROR: 'arguments' cannot be referenced in property initializers.
  }
}

function A1() {
  return new class T {
     a = arguments //~ERROR: 'arguments' cannot be referenced in property initializers.
  }
}

function B() {
  return class T {
     a = { b: arguments } //~ERROR: 'arguments' cannot be referenced in property initializers.
  }
}

function B1() {
  return new class T {
     a = { b: arguments } //~ERROR: 'arguments' cannot be referenced in property initializers.
  }
}

function C() {
  return class T {
     a = function () { arguments }
  }
}

function D() {
  return class T {
     a = () => arguments //~ERROR: 'arguments' cannot be referenced in property initializers.
  }
}

function D1() {
  return class T {
    a = () => {
      arguments;    //~ERROR: 'arguments' cannot be referenced in property initializers.
      const b = () => {
        return arguments;     //~ERROR: 'arguments' cannot be referenced in property initializers.
      }

      function f() {
        return arguments;      // ok
      }

      arguments; //~ERROR: 'arguments' cannot be referenced in property initializers.
    }
 }
}

function D2() {
  return class {
    constructor() {
      arguments;  // ok
    }
    get foo() {
      return arguments;  // ok
    }
    set foo(foo: any) {
      arguments;  // ok
    }
    bar() {
      arguments;  // ok
    }
    [Symbol.iterator]() {
      arguments;  // ok
    }
  }
}

function D3() {
  return class T {
    static {
      arguments;  //~ERROR: 'arguments' cannot be referenced in property initializers.
      while(1) {
        arguments //~ERROR: 'arguments' cannot be referenced in property initializers.
      }
    }
  }
}

function D4() {
  return class T {
    static {
      function f() {
        arguments;  // ok
      }
    }
  }
}


function D5() {
  return class T {
     a = (() => { return arguments; })()  //~ERROR: 'arguments' cannot be referenced in property initializers.
  }
}

function D6() {
  return class T {
     a = (x = arguments) => {}    //~ERROR: 'arguments' cannot be referenced in property initializers.
  }
}

function D7() {
  return class T {
     a(x = arguments){  // ok

     }
  }
}

class D8 {
  a = arguments;  
  //~^ ERROR: 'arguments' cannot be referenced in property initializers.
  //~| ERROR: Cannot find name 'arguments'.
}