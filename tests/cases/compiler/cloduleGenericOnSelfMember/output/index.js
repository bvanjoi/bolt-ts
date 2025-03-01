class ServiceBase {
  field
}
class Service extends ServiceBase {}

(function (Service) {

  var Base = {name: "1",
  value: 5};
  Service.Base = Base
  
})(Service);