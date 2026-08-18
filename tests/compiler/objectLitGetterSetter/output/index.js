// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/objectLitGetterSetter.ts`, Apache-2.0 License
var obj = {};
Object.defineProperty(obj, 'accProperty', ({
  get: function () {
    eval('public = 1;');
    return 11;
  },
  set: function (v) {}  
}));