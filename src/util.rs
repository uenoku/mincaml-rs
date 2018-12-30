use rpds::List;

pub fn destruct<T: Clone>(a: List<T>) -> Option<(T, List<T>)> {
    match a.first() {
        Some(y) => Some((y.clone(), a.drop_first().unwrap())),
        None => None,
    }
}
pub fn concat<T: Clone>(a: List<T>, b: List<T>) -> List<T> {
    match a.first() {
        Some(y) => concat(a.drop_first().unwrap(), b.push_front(y.clone())),
        None => b,
    }
}
pub fn concat_com<T: Clone>(a: List<T>, b: List<T>) -> List<T> {
    if a.len() > b.len() {
        concat(b, a)
    } else {
        concat(a, b)
    }
}
pub fn from_vec<T: Clone>(a: &Vec<T>) -> List<T> {
    let b = List::new();
    a.into_iter()
        .rev()
        .fold(b, |acc, i| acc.push_front(i.clone()))
}
pub fn map<T: Clone, B>(a: List<T>, f: Box<Fn(T) -> B>) -> List<B> {
    match destruct(a) {
        None => List::new(),
        Some((l, ls)) => {
            let g = f(l);
            (map(ls, f).push_front(g))
        }
    }
}
