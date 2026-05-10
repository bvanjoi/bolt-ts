function asObservable(input) {
  return typeof input === 'string' ? of(input) : from(input)
}