// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/noImplicitReturnsWithProtectedBlocks1.ts`, Apache-2.0 License
//@compiler-options: target=es2015
function main1() {
  try {
    return get();
  }finally {
    log('in finally');
  }
}