// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noCrashOnThisTypeUsage.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function notifyListeners(listenable, change) {}
export class ObservableValue {
  constructor(value) {
    var newValue = value;
    var oldValue = null;
    notifyListeners(this, {
          type: 'update',
      object: this,
      newValue,
      oldValue      
    });
    this.value = value
    }
  changeListeners = [];
  observe(handler, fireImmediately) {}
}