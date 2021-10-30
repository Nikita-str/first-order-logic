pub trait DeepCopy{ fn deep_copy(&self) -> Self; }

impl<T> DeepCopy for Vec<T> where T: DeepCopy{
    fn deep_copy(&self) -> Self {
        let mut vec = Self::with_capacity(self.len());
        for x in self { vec.push(x.deep_copy()) }
        vec
    }
}