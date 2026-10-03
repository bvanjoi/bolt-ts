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