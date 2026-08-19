function f1(foo) {
  if (foo.kind === 'a') {
    foo.a;
  }
  
}
function f2(foo) {
  if (foo.kind === 'a') {
    foo.a;
  }
  
}
var GatewayOpcode = {};
(function (GatewayOpcode) {

  GatewayOpcode[GatewayOpcode['DISPATCH'] = 0] = 'DISPATCH'
  GatewayOpcode[GatewayOpcode['HEARTBEAT'] = 1] = 'HEARTBEAT'
  GatewayOpcode[GatewayOpcode['IDENTIFY'] = 2] = 'IDENTIFY'
  GatewayOpcode[GatewayOpcode['PRESENCE_UPDATE'] = 3] = 'PRESENCE_UPDATE'
  GatewayOpcode[GatewayOpcode['VOICE_STATE_UPDATE'] = 4] = 'VOICE_STATE_UPDATE'
  GatewayOpcode[GatewayOpcode['RESUME'] = 6] = 'RESUME'
  GatewayOpcode[GatewayOpcode['RECONNECT'] = 7] = 'RECONNECT'
  GatewayOpcode[GatewayOpcode['REQUEST_GUILD_MEMBERS'] = 8] = 'REQUEST_GUILD_MEMBERS'
  GatewayOpcode[GatewayOpcode['INVALID_SESSION'] = 9] = 'INVALID_SESSION'
  GatewayOpcode[GatewayOpcode['HELLO'] = 10] = 'HELLO'
  GatewayOpcode[GatewayOpcode['HEARTBEAT_ACK'] = 11] = 'HEARTBEAT_ACK'
})(GatewayOpcode);
function assertMessage(event) {}
export async function adaptSession(input) {
  if (input.t === 'MESSAGE_CREATE') {
    assertMessage(input.d);
  }
  
}