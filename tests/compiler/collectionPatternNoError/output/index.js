// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collectionPatternNoError.ts`, Apache-2.0 License
class Message {
  clone() {
    return this;
  }
}
function fetchMsg(protoCtor) {
  return null;
}
class DataProvider {
  constructor(message, messageList) {}
  fetch() {
    var messageList = fetchMsg(this.messageList);
    messageList.methodOnMessageList();
  }
}
function f(message, messageList) {
  fetchMsg(messageList).methodOnMessageList();
}
function f0(n) {
  var g0 = g(n);
  g0.a1();
}