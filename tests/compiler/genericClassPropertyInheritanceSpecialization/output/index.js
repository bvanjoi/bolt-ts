// From `github.com/microsoft/TypeScript/blob/v6.0.3/tests/cases/compiler/genericClassPropertyInheritanceSpecialization.ts`, Apache-2.0 License
//@compiler-options: target=es2015
//@compiler-options: strict=false

var Portal = {};
(function (Portal) {

  var Controls = {};
  (function (Controls) {
  
    var Validators = {};
    (function (Validators) {
    
      class Validator {
        _subscription;
        message;
        validationState;
        validate;
        constructor(message) {}
        destroy() {}
        _validate(value) {
          return 0;
        }
      }
      Validators.Validator = Validator;
      
    })(Validators);
    Controls.Validators = Validators;
    
  })(Controls);
  Portal.Controls = Controls;
  
})(Portal);
var PortalFx = {};
(function (PortalFx) {

  var ViewModels = {};
  (function (ViewModels) {
  
    var Controls = {};
    (function (Controls) {
    
      var Validators = {};
      (function (Validators) {
      
        class Validator extends Portal.Controls.Validators.Validator {
          constructor(message) {super(message);}
        }
        Validators.Validator = Validator;
        
      })(Validators);
      Controls.Validators = Validators;
      
    })(Controls);
    ViewModels.Controls = Controls;
    
  })(ViewModels);
  PortalFx.ViewModels = ViewModels;
  
})(PortalFx);
class ViewModel {
  validators = ko.observableArray();
}