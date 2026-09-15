function isNonNullable(obj) {
  if (obj === undefined || obj === null) {
    throw new Error('Must not be a nullable value')
  }
  
}
export { isNonNullable }