// From `github.com/microsoft/TypeScript/blob/v5.8.2/tests/cases/compiler/fatarrowfunctionsInFunctions.ts`, Apache-2.0 License

var messenger = {message: "Hello World",
start: function () {
  var _self = this;
  setTimeout(function () {
    _self.message.toString();
  }, 0);
}};
messenger.start();