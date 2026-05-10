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