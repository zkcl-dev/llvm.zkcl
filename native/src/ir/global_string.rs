use super::{Global, GlobalOptions, Linkage, Module, UnnamedAddress};
use crate::Result;

impl Module {
    /// Add private, read-only, byte-aligned storage for these bytes.
    /// The caller can reuse this global through its opaque pointer address.
    pub fn add_global_string(
        &self,
        name: &str,
        bytes: &[u8],
        nul_terminated: bool,
    ) -> Result<Global> {
        let initializer = self.context().const_bytes(bytes, nul_terminated);
        let global = self.add_global(
            name,
            &initializer.ty(),
            GlobalOptions {
                initializer: Some(&initializer),
                constant: true,
                linkage: Linkage::Private,
            },
        )?;
        global.set_unnamed_address(UnnamedAddress::Global);
        global.set_alignment(1)?;
        Ok(global)
    }
}
