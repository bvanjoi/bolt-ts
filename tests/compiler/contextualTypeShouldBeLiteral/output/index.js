function foo(bar) {}
foo({
  type: 'y',
  value: 'done',
  method() {
    this;
    this.type;
    this.value;
  }  
});
function foo2(bar) {}
foo2({
  type2: 'y',
  value: 'done',
  method() {
    this;
    this.value;
  }  
});
var xy = {
  type: 'y',
  value: 11,
  ytra: 12  
};
xy;
var xyz = {
  x: 'x',
  y: 'y',
  value: 'foo',
  method() {
    this;
    this.x;
    this.y;
    this.value;
  }  
};
xyz;
var test = {
  items: {
      hello: {
          type: 'string'      
    },
    world: {
          items: {
              nested: {
                  type: 'string'          
        }        
      }      
    }    
  }  
};