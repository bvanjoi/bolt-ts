pub fn append_if_unique<'a, T>(array: &mut Vec<&'a T>, value: &'a T) {
    if array.iter().all(|item| !std::ptr::eq(item, &value)) {
        array.push(value);
    }
}
