var result;
openFile();
try {
  result = someOperation();
}finally {
  closeFile();
}
result;