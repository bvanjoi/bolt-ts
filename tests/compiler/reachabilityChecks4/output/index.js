// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/reachabilityChecks4.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false
//@compiler-options: noFallthroughCasesInSwitch
function foo(x, y) {
  switch (x) {
    case 1:
    case 2:
      return 1;
    
    case 3:
      if (y) {
        return 2;
      }
      
    
    case 4:
      return 3;
    
  }
}
function f1(x) {
  switch (x) {
    case 0:
      fail();
    
    case 1:
      noop();
    
    case 2:
      return ;
    
  }
}// Repro from #34021

function f2(transition) {
  switch (transition.behavior) {
    case 'SLIDE':
      switch (transition.direction) {
        case 'LEFT':
          return [];
        
        case 'RIGHT':
          return [];
        
        case 'TOP':
          return [];
        
        case 'BOTTOM':
          return [];
        
      }
    
    case 'SLIDE_OUT':
      switch (transition.direction) {
        case 'LEFT':
          return [];
        
        case 'RIGHT':
          return [];
        
        case 'TOP':
          return [];
        
        case 'BOTTOM':
          return [];
        
      }
    
  }
}