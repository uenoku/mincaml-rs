pub enum Wr_Inst {
    Write(to_loop::GlobalWR),
    Read(to_loop::GlobalWR),
}
pub enum Wr_Cond {
    GlobalCmp(syntax::Cmp, String, Vec<to_loop::Index>)
    Other,
}
pub enum Wr_ControlFlow {
    Jump (String), 
    Return ,
    Branch(Wr_Cond, String, String)
}
pub struct Wr_Block {
    inst:Vec<Wr_Inst>,
    label: Wr_ControlFlow,
}
