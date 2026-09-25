import a from './a'
var b = {
  f: async () => {
    await a.f();
  }  
};