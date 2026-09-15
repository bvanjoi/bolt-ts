import b from './b'
var a = {
  f: async () => {
    await b.f();
  }  
};