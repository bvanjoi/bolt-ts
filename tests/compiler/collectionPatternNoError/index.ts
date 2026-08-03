// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/collectionPatternNoError.ts`, Apache-2.0 License

//@compiler-options: target=es2015

interface MsgConstructor<T extends Message> {
  new(data: Array<{}>): T;
}
class Message {
  clone(): this {
    return this;
  }
}
interface MessageList<T extends Message> extends Message {
  methodOnMessageList(): T[];
}

function fetchMsg<V extends Message>(protoCtor: MsgConstructor<V>): V {
  return null!;
}

class DataProvider<T extends Message, U extends MessageList<T>> {
  constructor(
    private readonly message: MsgConstructor<T>,
    private readonly messageList: MsgConstructor<U>,
  ) { }

  fetch() {
    const messageList = fetchMsg(this.messageList);
    messageList.methodOnMessageList();
  }
}

// The same bug as the above but using indexed accesses
// (won't surface directly unless unsound indexed access assignments are forbidden)
function f<
  U extends {TType: MessageList<T>},
  T extends Message
>(message: MsgConstructor<T>, messageList: MsgConstructor<U["TType"]>) {
  fetchMsg(messageList).methodOnMessageList();
}

interface A0 {
  a0(): this;
}

interface A1 extends A0 {
  a1(): void;
}

declare function g<V extends A0>(n: V): V;

function f0<U extends A1>(n: U) {
  const g0: A1 = g(n);
  g0.a1();
}
