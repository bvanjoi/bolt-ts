export function doRemove(dds) {
  if (!Array.isArray(dds)) {
    dds = [dds];
  }
  
  for ( var n of dds) {
    n.d();
  }
  return dds
}