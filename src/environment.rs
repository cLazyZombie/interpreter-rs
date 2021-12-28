use std::collections::HashMap;

use crate::{object::Object, token::IdentToken};

#[derive(Debug)]
pub struct Environment {
    store: HashMap<IdentToken, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
        }
    }

    pub fn set(&mut self, ident: IdentToken, obj: Object) {
        self.store.insert(ident, obj);
    }

    pub fn get(&self, ident: &IdentToken) -> Option<Object> {
        self.store.get(ident).map(|obj| obj.clone())
    }
}
