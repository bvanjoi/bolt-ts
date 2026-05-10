// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/matchReturnTypeInAllBranches.ts`, Apache-2.0 License

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

}

var m : _modes;
//~^ ERROR: Cannot find name '_modes'.

