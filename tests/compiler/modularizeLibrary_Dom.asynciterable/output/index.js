navigator.storage.getDirectory().then(async (directory) => {
  for await ( var [key, handle] of directory) {
    handle.kind;
  }
});