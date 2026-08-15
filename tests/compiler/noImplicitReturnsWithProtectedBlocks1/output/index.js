function main1() {
  try {
    return get();
  }finally {
    log('in finally');
  }
}