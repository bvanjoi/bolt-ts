class X {
  y = 0
  getSettings(keys) {
    var ret;
    return ret.always(() => {
      this.y = 0;
    }).promise()
  }
}