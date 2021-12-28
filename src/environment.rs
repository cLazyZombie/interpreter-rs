use std::collections::HashMap;

use crate::{object::Object, token::IdentToken};

#[derive(Debug)]
pub struct Environment<'a> {
    store: HashMap<IdentToken, Object>,
    parent: Option<&'a Environment<'a>>,
}

impl<'a> Environment<'a> {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            parent: None,
        }
    }

    pub fn set(&mut self, ident: IdentToken, obj: Object) {
        self.store.insert(ident, obj);
    }

    pub fn set_parent(&mut self, parent: &'a Environment) {
        self.parent = Some(parent);
    }

    pub fn get(&self, ident: &IdentToken) -> Option<Object> {
        if let Some(obj) = self.store.get(ident).map(|obj| obj.clone()) {
            Some(obj)
        } else {
            if let Some(parent) = self.parent {
                parent.get(ident)
            } else {
                None
            }
        }
    }
}
