// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/moduleVisibilityTest3.ts`, Apache-2.0 License

//@compiler-options: target=es2015

namespace _modes {
 export interface IMode {
  
 }
 
 class Mode {
  
 }
}

//_modes. // produces an internal error - please implement in derived class

namespace editor {
 import modes = _modes;
 
 var i : modes.IMode;
  
 // If you just use p1:modes, the compiler accepts it - should be an error
 class Bug {
     constructor(p1: modes, p2: modes.Mode) {// should be an error on p2 - it's not exported
         //~^ ERROR: Namespace '_modes' has no exported member 'Mode'.
         //~| ERROR: Cannot find name 'modes'.
         var x:modes.Mode;
         //~^ ERROR: Namespace '_modes' has no exported member 'Mode'.
     }
    
 }
}
