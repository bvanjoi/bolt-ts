var createService = (ServiceCtr) => {
  Object.keys(ServiceCtr).forEach((key) => {
    var method = (ServiceCtr)[key];
    var {__$daemonMode, __$action, id} = method;
  });
};