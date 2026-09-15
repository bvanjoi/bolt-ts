function call(obj, cb) {
  cb(obj);
}

call(obj, ({foo, ...rest}) => {
  console.log(rest.bar);
});