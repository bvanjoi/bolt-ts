class TestFile {
  message;
  name;
  constructor(message) {var getMessage = () => (message + this.name);
    this.message = getMessage();}
}