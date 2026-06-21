function goo1(x) {
  if (x.kind === 'A' && x.foo !== undefined) {
    x.foo.length;
  }
  
}
function goo2(x) {
  if (x.foo !== undefined && x.kind === 'A') {
    x.foo.length;
  }
  
}
function foo1(x) {
  if (x.bar && x.foo !== undefined) {
    x.foo.length;
  }
  
}
function foo2(x) {
  if (x.foo !== undefined && x.bar) {
    x.foo.length;
  }
  
}
function foo3(x) {
  if (x.baz && x.foo !== undefined) {
    x.foo.length;
  }
  
}
function foo4(x) {
  if (x.foo !== undefined && x.baz) {
    x.foo.length;
  }
  
}
function foo5(x) {
  if (x.qux && x.foo !== undefined) {
    x.foo.length;
  }
  
}
function foo6(x) {
  if (x.foo !== undefined && x.qux) {
    x.foo.length;
  }
  
}
var Types = {};
(function (Types) {

  Types[Types['Str'] = 1] = 'Str'
  Types[Types['Num'] = 2] = 'Num'
})(Types);
function func2(inst) {
  while (true) {
    switch (inst.type) {
      case Types.Str:
        {
          inst.value.length;
          break;
        }
      
      case Types.Num:
        {
          inst.value.toExponential;
          break;
        }
      
    }
  }
}
var f = (_a, _b) => {};
var u = {};
u.a && u.b && f(u.a, u.b);
u.b && u.a && f(u.a, u.b);
export function foo(obj) {
  switch (obj.key) {
    case '+':
      {
        onlyPlus(obj.key);
        return 
      }
    
  }
}
function onlyPlus(arg) {
  return arg
}
var BarEnum = {};
(function (BarEnum) {

  BarEnum[BarEnum['bar1'] = 1] = 'bar1'
  BarEnum[BarEnum['bar2'] = 2] = 'bar2'
})(BarEnum);
function func3(value) {
  if (value.type !== undefined) {
    switch (value.type) {
      case BarEnum.bar1:
        break;
      
      case BarEnum.bar2:
        break;
      
      default:
        never(value.type);
      
    }
  }
  
}
function WorksProperly(data) {
  if (data.Name === 'TypeA') {
    var value1 = data.Value1;
  }
  
}
function DoesNotWork(data) {
  if (isType(data)) {
    if (data.Name === 'TypeA') {
      var value1 = data.Value1;
    }
    
  }
  
}
var doTestingStuff = (mapOfTests, ids) => {
  ids.forEach((id) => {
    var test;
    test = mapOfTests[id];
    if (test.type === 'testA') {
      console.log(test.bananas);
    }
    
    switch (test.type) {
      case 'testA':
        {
          console.log(test.bananas);
        }
      
    }
  });
};