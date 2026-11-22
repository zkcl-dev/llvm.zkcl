use std::collections::HashSet;

use super::{Block, Function};

impl Block {
    /// Outgoing control-flow edges in the terminator's successor order.
    /// Parallel edges repeat the destination. An unterminated block has none.
    pub fn successors(&self) -> impl Iterator<Item = Block> + use<> {
        self.last_instruction()
            .into_iter()
            .flat_map(|terminator| terminator.successors())
    }

    /// Incoming control-flow edges in function block order. A source appears
    /// once for each edge to this block, including from unreachable blocks.
    pub fn predecessors(&self) -> impl Iterator<Item = Block> {
        let destination = self.raw;
        self.function.blocks().flat_map(move |source| {
            source
                .successors()
                .filter(move |successor| successor.raw == destination)
                .map(move |_| source.clone())
        })
    }
}

impl Function {
    /// Reverse postorder from the entry, visiting successors in terminator order.
    /// A declaration yields an empty list. Each reachable block appears once.
    pub fn reverse_postorder(&self) -> Vec<Block> {
        let Some(entry) = self.first_block() else {
            return Vec::new();
        };
        let mut seen = HashSet::from([entry.raw]);
        let mut stack = vec![(entry.successors(), entry)];
        let mut postorder = Vec::new();
        while let Some((successors, _)) = stack.last_mut() {
            if let Some(block) = successors.find(|block| seen.insert(block.raw)) {
                stack.push((block.successors(), block));
            } else {
                let (_, block) = stack.pop().expect("nonempty DFS stack");
                postorder.push(block);
            }
        }
        postorder.reverse();
        postorder
    }
}
