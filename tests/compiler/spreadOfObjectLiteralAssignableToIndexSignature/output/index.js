var foo = {};
var bar = {
  ...(foo !== undefined && {
      foo    
  })  
};
var recordOfRecords = {};
recordOfRecords.propA = {
  ...(foo !== undefined ? {
      foo    
  } : {})  
};
recordOfRecords.propB = {
  ...(foo && {
      foo    
  })  
};
recordOfRecords.propC = {
  ...(foo !== undefined && {
      foo    
  })  
};
var recordsOfRecordsOrEmpty = {};
recordsOfRecordsOrEmpty.propA = {
  ...(foo !== undefined ? {
      foo    
  } : {})  
};
recordsOfRecordsOrEmpty.propB = {
  ...(foo && {
      foo    
  })  
};
recordsOfRecordsOrEmpty.propC = {
  ...(foo !== undefined && {
      foo    
  })  
};