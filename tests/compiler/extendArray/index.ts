// From `github.com/microsoft/TypeScript/blob/v6.0.2/tests/cases/compiler/extendArray.ts`, Apache-2.0 License

//@compiler-options: target=es2015
//@compiler-options: strict=false

var a = [1,2];
a.forEach(function (v,i,a) {});


declare namespace _Core {
  interface Array {
    collect(fn:(e:_element) => _element[]) : any[];
    //~^ ERROR: Cannot find name '_element'.
    //~| ERROR: Cannot find name '_element'.
  }
}


var arr = (<any>Array).prototype;
arr.collect = function (fn) {
    var res = [];
    for (var i = 0; i < this.length; ++i) {
        var tmp = fn(this[i]);
        for (var j = 0; j < tmp.length; ++j) {
            res.push(tmp[j]);
        }
    }
    return res;
};

