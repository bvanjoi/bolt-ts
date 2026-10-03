export function getPrismaClient(options) {
  class PrismaClient {
    self;
    constructor(options) {return (this.self = applyModelsAndClientExtensions(this));}
  }
  return PrismaClient;
}
export function applyModelsAndClientExtensions(client) {
  return client;
}