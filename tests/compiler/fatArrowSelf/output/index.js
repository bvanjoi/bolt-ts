var Events = {};
(function (Events) {

  class EventEmitter {
    addListener(type, listener) {}
  }
  Events.EventEmitter = EventEmitter;
  
})(Events);
var Consumer = {};
(function (Consumer) {

  class EventEmitterConsummer {
    constructor(emitter) {}
    register() {
      this.emitter.addListener('change', (e) => {
        this.changed();
      });
    }
    changed() {}
  }
  
})(Consumer);