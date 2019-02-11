use crate::ir;
use crate::knormal;
use crate::syntax;
use rpds::HashTrieSet;
use std::collections::HashSet;
static ENTRY_INDEX: usize = 1;
pub fn f(p: Vec<ir::Fundef>) -> Vec<ir::Fundef> {
    let p: Vec<_> = p.into_iter().map(|x| x.replace_self_rec_block()).collect();
    for i in &p {
        let x = i.get_loop_idx();
        //       let hoge = i.only_iter_is_to_loop();
        //        let t = i.collect_wrinfo(&p);
        if x.is_some() {
            info!("{}", i.name.0);
            i.is_loop_dependent(&p);
        }
    }
    p
}
type Functions = Vec<ir::Fundef>;

pub fn have_some_wr(s: &String, functions: &Functions) -> bool {
    match get_function_ref_opt(functions, s) {
        Some(x) => {
            for i in &x.blocks {
                if x.get_global_wr_by_block_ref(i, functions).len() > 0 {
                    return true;
                }
            }

            false
        }
        None => false,
    }
}
pub fn get_function_ref_opt<'a>(functions: &'a Functions, s: &String) -> Option<&'a ir::Fundef> {
    for i in functions {
        if *i.name.0 == *s {
            return Some(i);
        }
    }
    None
}
#[derive(Debug, Clone)]
pub enum Index {
    Const(i32),
    Unkown,
    Iter(i32),
}
#[derive(Debug, Clone)]
pub enum Arg {
    Iter(i32),
    Ptr(GlobalWR),
    Const(i32),
    Unkown,
    Arg(usize),
}
#[derive(Debug, Clone)]
pub enum WRInfo {
    Write(GlobalWR),
    Read(GlobalWR),
    Call(String, Vec<Arg>),
}
impl WRInfo {
    pub fn is_write(&self) -> bool {
        match self {
            WRInfo::Write(_) => true,
            _ => false,
        }
    }
    pub fn have_undef(&self) -> bool {
        match self {
            WRInfo::Write((x, y)) | WRInfo::Read((x, y)) => y.iter().any(|x| match x {
                Index::Unkown => true,
                _ => false,
            }),
            _ => true,
        }
    }
}
#[derive(Debug, Clone)]
pub enum Ptr {
    Global(String),
    Arg(usize),
    Local,
}
type GlobalWR = (Ptr, Vec<Index>);
pub fn global_wr_to_constant_eq(x: &GlobalWR, (y, z): &(String, Vec<usize>)) -> bool {
    match x {
        (Ptr::Global(s), x) if *s == *y => {
            for i in 0..x.len() {
                match x[i] {
                    Index::Const(y) if z[i] as i32 != y => {
                        return false;
                    }
                    Index::Const(y) => {}
                    _ => {
                        return false;
                    }
                }
            }
            true
        }
        _ => false,
    }
}

pub fn convert_to_arg(
    v: &Vec<knormal::Var>,
    fun: &ir::Fundef,
    iter: &String,
    functions: &Functions,
) -> Vec<Arg> {
    let mut ans = vec![];
    debug!("{} {}", fun.name.0, iter);
    for i in v {
        match i {
            knormal::Var::OpVar(x, y) if *x == *iter => ans.push(Arg::Iter(0)),
            knormal::Var::OpVar(x, y) => match fun.get_instruction(x) {
                Some(y) => match y {
                    ir::Inst::Load { dst, ptr, idx } => {
                        match get_global_wr(fun, iter, &ptr, &idx) {
                            Some(x) => ans.push(Arg::Ptr(x)),
                            None => ans.push(Arg::Unkown),
                        }
                    }
                    ir::Inst::BinaryRR {
                        opcode,
                        rhs,
                        lhs,
                        dst,
                    } if (opcode == ir::OpBinaryRR::Add || opcode == ir::OpBinaryRR::Sub) => {
                        if opcode == ir::OpBinaryRR::Add {
                            if lhs.is_name_eq(iter) && rhs.is_constant() {
                                ans.push(Arg::Iter(rhs.get_signedimm().unwrap()));
                            } else if rhs.is_name_eq(iter) && lhs.is_constant() {
                                ans.push(Arg::Iter(lhs.get_signedimm().unwrap()));
                            }
                        } else if opcode == ir::OpBinaryRR::Sub {
                            if lhs.is_name_eq(iter) && rhs.is_constant() {
                                ans.push(Arg::Iter(-rhs.get_signedimm().unwrap()));
                            } else if rhs.is_name_eq(iter) && lhs.is_constant() {
                                ans.push(Arg::Unkown);
                            }
                        }
                    }
                    _ => {
                        ans.push(Arg::Unkown);
                    }
                },
                None => match fun.get_arg_idx(x) {
                    Some(y) => ans.push(Arg::Arg(y)),
                    None => (),
                },
            },
            knormal::Var::Ext(s, y) => ans.push(Arg::Ptr((Ptr::Global(s.to_string()), vec![]))),
            knormal::Var::Constant(syntax::Const::CInt(i)) => ans.push(Arg::Const(*i)),
            _ => ans.push(Arg::Unkown),
        }
    }
    ans
}
pub fn get_global_wr(
    fun: &ir::Fundef,
    iter: &String,
    ptr: &knormal::Var,
    idx: &knormal::Var,
) -> Option<GlobalWR> {
    match fun.get_global_load(ptr, iter) {
        Some((glb, mut index)) => match idx {
            knormal::Var::OpVar(x, y) if *x == *iter => {
                index.push(Index::Iter(0));
                Some((glb, index))
            }
            knormal::Var::OpVar(x, y) => match fun.get_instruction(x) {
                Some(y) => match y {
                    ir::Inst::BinaryRR {
                        opcode,
                        rhs,
                        lhs,
                        dst,
                    } if (opcode == ir::OpBinaryRR::Add || opcode == ir::OpBinaryRR::Sub) => {
                        if opcode == ir::OpBinaryRR::Add {
                            if lhs.is_name_eq(iter) && rhs.is_constant() {
                                index.push(Index::Iter(rhs.get_signedimm().unwrap()));
                            } else if rhs.is_name_eq(iter) && lhs.is_constant() {
                                index.push(Index::Iter(lhs.get_signedimm().unwrap()));
                            }
                        } else if opcode == ir::OpBinaryRR::Sub {
                            if lhs.is_name_eq(iter) && rhs.is_constant() {
                                index.push(Index::Iter(-rhs.get_signedimm().unwrap()));
                            } else if rhs.is_name_eq(iter) && lhs.is_constant() {
                                index.push(Index::Unkown);
                            }
                        }
                        Some((glb, index))
                    }
                    _ => {
                        index.push(Index::Unkown);
                        Some((glb, index))
                    }
                },
                None => {
                    index.push(Index::Unkown);
                    Some((glb, index))
                }
            },
            knormal::Var::Constant(syntax::Const::CInt(i)) => {
                index.push(Index::Const(*i));
                Some((glb, index))
            }
            _ => unreachable!(),
        },
        // None if fun.get_arg_idx(ptr).is_some() => {
        //     Some((Ptr::Arg(fun.get_arg_idx(ptr).unwrap()), vec![]))
        // }
        None => None,
    }
}
impl ir::Inst {
    pub fn global_store(&self, fun: &ir::Fundef, iter: &String) -> Option<GlobalWR> {
        match self {
            ir::Inst::Store { src, ptr, idx } => get_global_wr(fun, iter, ptr, idx),
            _ => None,
        }
    }
    pub fn global_load(&self, fun: &ir::Fundef, iter: &String) -> Option<GlobalWR> {
        match self {
            ir::Inst::Load { dst, ptr, idx } => get_global_wr(fun, iter, ptr, idx),
            _ => None,
        }
    }
    pub fn global_call_simple(
        &self,
        fun: &ir::Fundef,
        iter: &String,
        functions: &Functions,
    ) -> Option<Vec<WRInfo>> {
        match self {
            ir::Inst::CallDir { label, dst, args } => {
                match get_function_ref_opt(functions, &label.0) {
                    Some(y) if y.is_simple_function() => None,
                    _ => None,
                }
            }
            _ => None,
        }
    }

    pub fn global_call(
        &self,
        fun: &ir::Fundef,
        iter: &String,
        functions: &Functions,
    ) -> Option<WRInfo> {
        match self {
            ir::Inst::CallDir { label, dst, args } if label.0 == fun.name.0 => Some(WRInfo::Call(
                label.0.clone(),
                convert_to_arg(args, fun, iter, functions),
            )),

            ir::Inst::CallDir { label, dst, args } if have_some_wr(&label.0, functions) => Some(
                WRInfo::Call(label.0.clone(), convert_to_arg(args, fun, iter, functions)),
            ),
            _ => None,
        }
    }
    pub fn has_io(&self, f: &String, functions: &Functions) -> bool {
        match self {
            ir::Inst::CallDir { label, args, dst } => {
                if label.0.as_str() == "read_int"
                    || label.0.as_str() == "read_float"
                    || label.0.as_str() == "print_char"
                {
                    true
                } else if *label.0 == *f {
                    false
                } else {
                    match get_function_ref_opt(functions, &label.0) {
                        Some(y) => y.has_io(functions),
                        None => false,
                    }
                }
            }
            _ => false,
        }
    }
}

impl ir::Block {
    pub fn get_global_wr(
        &self,
        fun: &ir::Fundef,
        iter: &String,
        functions: &Functions,
    ) -> Vec<WRInfo> {
        let mut ans = vec![];
        for i in &self.inst {
            match i.global_load(fun, iter) {
                Some(y) => ans.push(WRInfo::Read(y)),
                None => (),
            }
            match i.global_store(fun, iter) {
                Some(y) => ans.push(WRInfo::Write(y)),
                None => (),
            }
            match i.global_call(fun, iter, functions) {
                Some(y) => ans.push(y),
                None => (),
            }
        }
        ans
    }
    pub fn get_global_load(&self, fun: &ir::Fundef, iter: &String) -> Vec<GlobalWR> {
        let mut ans = vec![];
        for i in &self.inst {
            match i.global_load(fun, iter) {
                Some(y) => ans.push(y),
                None => (),
            }
        }
        ans
    }
    pub fn get_instruction(&self, x: &String) -> Option<ir::Inst> {
        for i in &self.inst {
            match i.dest() {
                Some((y, z)) if *x == y => return Some(i.clone()),
                _ => (),
            }
        }
        None
    }
    pub fn get_last_instruction(&self) -> Option<ir::Inst> {
        self.inst.iter().cloned().last()
    }
    pub fn is_no_branch_block(&self, fun: &ir::Fundef) -> bool {
        // もうbranchがない
        match &self.last {
            ir::ControlFlow::Return(_) => true,
            ir::ControlFlow::Jump(y) => fun.get_block_ref(y).is_no_branch_block(fun),
            _ => false,
        }
    }
    // blockでReturnするか
    pub fn is_return_block(&self, fun: &ir::Fundef) -> bool {
        match &self.last {
            ir::ControlFlow::Return(_) => true,
            _ => false,
        }
    }
    pub fn has_io(&self, f: &String, functions: &Vec<ir::Fundef>) -> bool {
        self.inst.iter().any(|x| x.has_io(f, functions))
    }
    // 飛ぶ先の任意のブロックで命令が行われずlastがJump|Returnである
    pub fn is_tail_block(&self, fun: &ir::Fundef) -> bool {
        match &self.last {
            ir::ControlFlow::Jump(y) if self.inst.is_empty() => {
                fun.get_block_ref(y).is_tail_block(fun)
            }
            ir::ControlFlow::Return(_) if self.inst.is_empty() => true,
            _ => false,
        }
    }
    pub fn get_self_rec_call(
        &self,
        current_fun_name: &String,
        fun: &ir::Fundef,
    ) -> Option<ir::Inst> {
        match &self.last {
            ir::ControlFlow::Return(Some(knormal::Var::OpVar(x, y))) => {
                let inst = self.get_instruction(&x);
                match inst {
                    None => None,
                    Some(y) => match &y {
                        ir::Inst::CallDir { dst, label, args } if *label.0 == *current_fun_name => {
                            //info!("{}", label.0);
                            Some(y)
                        }
                        _ => None,
                    },
                }
            }

            ir::ControlFlow::Return(None) => {
                let inst = self.get_last_instruction();
                //info!("last {:?}", inst);
                match inst {
                    None => None,
                    Some(y) => match &y {
                        ir::Inst::CallDir { dst, label, args } if *label.0 == *current_fun_name => {
                            //info!("{}", label.0);
                            Some(y)
                        }
                        _ => None,
                    },
                }
            }
            ir::ControlFlow::Jump(y) if fun.get_block_ref(y).is_tail_block(fun) => {
                let inst = self.get_last_instruction();
                //info!("last {:?}", inst);
                match inst {
                    None => None,
                    Some(y) => match &y {
                        ir::Inst::CallDir { dst, label, args } if *label.0 == *current_fun_name => {
                            //info!("{}", label.0);
                            Some(y)
                        }
                        _ => None,
                    },
                }
            }
            _ => None,
        }
    }

    pub fn is_self_rec_call(&self, current_fun_name: &String, fun: &ir::Fundef) -> bool {
        self.get_self_rec_call(current_fun_name, fun).is_some()
    }
}
type Blocks = Vec<ir::Block>;
impl ir::Fundef {
    pub fn is_simple_function(&self) -> bool {
        // ブロックが2個
        self.blocks.len() == 2
    }
    pub fn collect_wrinfo(&self, functions: &Functions) -> Vec<(String, String, Vec<WRInfo>)> {
        let mut ans = vec![];
        for i in &self.blocks {
            ans.push((
                self.name.0.clone(),
                i.label.clone(),
                self.get_global_wr_by_block_ref(i, functions),
            ));
        }
        ans
    }
    pub fn get_global_wr_by_block_ref(
        &self,
        block: &ir::Block,
        functions: &Functions,
    ) -> Vec<WRInfo> {
        let iter = self.get_loop_idx();
        match iter {
            Some((iter, y, z)) => block.get_global_wr(self, &iter, functions),
            None => block.get_global_wr(self, &"".to_string(), functions),
        }
    }
    pub fn get_global_load_by_block_ref(&self, block: &ir::Block) -> Vec<GlobalWR> {
        let iter = self.get_loop_idx();
        match iter {
            Some((iter, y, z)) => block.get_global_load(self, &iter),
            None => block.get_global_load(self, &"".to_string()),
        }
    }
    pub fn get_global_load(&self, ptr: &knormal::Var, iter: &String) -> Option<GlobalWR> {
        match ptr {
            knormal::Var::OpVar(x, y) => {
                let inst = self.get_instruction(x);
                match inst {
                    Some(y) => y.global_load(self, iter),
                    None =>
                    // arg
                    {
                        let idx = self.get_arg_idx(x);
                        match idx {
                            Some(idx) => Some((Ptr::Arg(idx), vec![])),
                            None => None,
                        }
                    }
                }
            }
            knormal::Var::Ext(x, y) => Some((Ptr::Global(x.to_string()), vec![])),
            _ => unreachable!(),
        }
    }
    pub fn get_instruction(&self, name: &String) -> Option<ir::Inst> {
        for i in &self.blocks {
            match i.get_instruction(name) {
                Some(y) => return Some(y),
                None => (),
            }
        }
        None
    }
    pub fn get_block_ref(&self, name: &String) -> &ir::Block {
        for i in &self.blocks {
            if *name == *i.label {
                return i;
            }
        }
        unreachable!()
    }
    pub fn get_instructions_by_operand(&self, name: &String) -> Vec<ir::Inst> {
        let mut ans = vec![];
        for block in &self.blocks {
            let inst = &block.inst;
            for i in inst {
                if i.gen().contains(name) {
                    ans.push(i.clone());
                }
            }
        }
        ans
    }
    fn has_loop_sub(&self, s: &String, mem: &HashTrieSet<String>) -> bool {
        if mem.contains(s) {
            true
        } else {
            let nmem = &mem.insert(s.clone());
            match &self.get_block_ref(s).last {
                ir::ControlFlow::Return(_) => false,
                ir::ControlFlow::Branch(a, b, c) => {
                    self.has_loop_sub(b, nmem) || self.has_loop_sub(c, nmem)
                }
                ir::ControlFlow::Jump(a) => self.has_loop_sub(a, nmem),
            }
        }
    }
    pub fn has_loop(&self, s: &String) -> bool {
        // ほんとはSCCするべきなんだけどめんどいので適当(指数時間)たかだか10個もないっしょ！
        self.has_loop_sub(s, &HashTrieSet::new())
    }
    pub fn break_cond_opt(&self, s: &String) -> Option<ir::Inst> {
        // あるbの真理値によってでループから脱出する,bの定義がiterと定数(ループ不変であればよい)の比較であればよい
        // つまり branch (b, entry', hoge)みたいなやつを見る
        // めんどいのでBranchで飛ぶ先がjumpもしくはReturnする場合としてしまう

        for block in &self.blocks {
            match &block.last {
                ir::ControlFlow::Branch(knormal::Var::OpVar(x, y), ifthen, ifelse)
                    if (self.has_loop(ifthen)
                        && self.get_block_ref(&ifelse).is_no_branch_block(&self))
                        || (self.has_loop(ifelse)
                            && self.get_block_ref(&ifthen).is_no_branch_block(&self)) =>
                {
                    match self.get_instruction(&x) {
                        Some(ir::Inst::BinaryRR {
                            opcode,
                            lhs,
                            rhs,
                            dst,
                        }) if (lhs.is_name_eq(s) && rhs.is_constant())
                            || (rhs.is_name_eq(s) && lhs.is_constant()) =>
                        {
                            match opcode {
                                ir::OpBinaryRR::Cond(y) => {
                                    return self.get_instruction(&x).clone();
                                }
                                _ => (),
                            }
                        }
                        _ => (),
                    }
                }
                _ => {}
            }
        }

        None
    }
    pub fn get_loop_idx(&self) -> Option<(String, i32, ir::Inst)> {
        let idx = self.get_idx();
        let mut candicate = vec![];
        for (s, offset) in &idx {
            let x = self.break_cond_opt(s);
            match x {
                Some(y) => candicate.push(Some((s.clone(), *offset, y.clone()))),
                None => (),
            }
        }
        if candicate.len() == 1 {
            candicate[0].clone()
        } else {
            None
        }
    }
    pub fn get_idx(&self) -> Vec<(String, i32)> {
        // entry'は1!
        let phis = &self.blocks[ENTRY_INDEX].phis;
        let mut iters = vec![];
        for i in phis {
            let name = &i.dst.0;
            for (var, label) in &i.args {
                if label.as_str() == "entry" {
                    continue;
                }
                match var {
                    knormal::Var::OpVar(x, y) => {
                        let inst = self.get_instruction(x);
                        match inst {
                            Some(ir::Inst::BinaryRR {
                                opcode,
                                dst,
                                lhs,
                                rhs,
                            }) if opcode == ir::OpBinaryRR::Add
                                || opcode == ir::OpBinaryRR::Sub =>
                            {
                                if opcode == ir::OpBinaryRR::Add {
                                    if (lhs.is_constant() && rhs.is_name_eq(name)) {
                                        iters.push((i.dst.0.clone(), lhs.get_signedimm().unwrap()))
                                    } else if (rhs.is_constant() && lhs.is_name_eq(name)) {
                                        iters.push((i.dst.0.clone(), rhs.get_signedimm().unwrap()))
                                    }
                                };
                                if opcode == ir::OpBinaryRR::Sub {
                                    if (lhs.is_constant() && rhs.is_name_eq(name)) {
                                        // これはだめ
                                    } else if (rhs.is_constant() && lhs.is_name_eq(name)) {
                                        iters.push((i.dst.0.clone(), -rhs.get_signedimm().unwrap()))
                                    }
                                };
                            }
                            _ => (),
                        }
                    }
                    _ => (),
                }
            }
        }
        //info!("name : {:?} iters {:?}", self.name, iters);
        iters
    }
    pub fn has_io(&self, functions: &Vec<ir::Fundef>) -> bool {
        // あるブロックが存在してioを含む(これはめんどいので保守的に)
        self.blocks
            .iter()
            .any(|x| x.has_io(&self.name.0, functions))
    }
    pub fn only_iter_is_to_loop(&self) -> bool {
        // iter以外はループを繰り越さない
        let iters = self.get_loop_idx();
        match iters {
            Some((x, y, z)) => self.blocks[ENTRY_INDEX].phis.len() == 1,
            _ => false,
        }
    }
    pub fn collect_all(&self, functions: &Functions) -> Vec<(String, String, Vec<WRInfo>)> {
        let mut args = vec![];
        for i in 0..self.args.len() {
            args.push(Arg::Arg(i));
        }
        self.collect_all_sub(functions, args)
    }

    pub fn collect_all_sub(
        &self,
        functions: &Functions,
        args: Vec<Arg>,
    ) -> Vec<(String, String, Vec<WRInfo>)> {
        let wrinfo = self.collect_wrinfo(functions);
        let mut rec_call = false;
        let mut ans = vec![];
        for (name, label, writes) in wrinfo {
            let mut tmp = vec![];
            for i in writes {
                match i {
                    WRInfo::Read((Ptr::Arg(idx), y)) => match args[idx].clone() {
                        Arg::Ptr((Ptr::Global(s), z)) => {
                            let mut tmp2 = z.clone();
                            y.iter().for_each(|x| tmp2.push(x.clone()));
                            tmp.push(WRInfo::Read((Ptr::Global(s), tmp2)));
                        }
                        Arg::Ptr((Ptr::Arg(s), z)) => {
                            let mut tmp2 = z.clone();
                            y.iter().for_each(|x| tmp2.push(x.clone()));
                            tmp.push(WRInfo::Read((Ptr::Arg(s), tmp2)));
                        }
                        Arg::Arg(i) => {
                            tmp.push(WRInfo::Read((Ptr::Arg(i), y)));
                        }
                        _ => {
                            tmp.push(WRInfo::Read((Ptr::Local, y)));
                        }
                    },
                    WRInfo::Read((Ptr::Global(x), y)) => {
                        tmp.push(WRInfo::Read((Ptr::Global(x), y)));
                    }
                    WRInfo::Write((Ptr::Arg(idx), y)) => match args[idx].clone() {
                        Arg::Ptr((Ptr::Global(s), z)) => {
                            let mut tmp2 = z.clone();
                            y.iter().for_each(|x| tmp2.push(x.clone()));
                            tmp.push(WRInfo::Write((Ptr::Global(s), tmp2)));
                        }
                        Arg::Ptr((Ptr::Arg(s), z)) => {
                            let mut tmp2 = z.clone();
                            y.iter().for_each(|x| tmp2.push(x.clone()));
                            tmp.push(WRInfo::Write((Ptr::Arg(s), tmp2)));
                        }
                        Arg::Arg(i) => {
                            tmp.push(WRInfo::Write((Ptr::Arg(i), y)));
                        }
                        _ => {
                            tmp.push(WRInfo::Write((Ptr::Local, y)));
                        }
                    },
                    WRInfo::Write((Ptr::Global(x), y)) => {
                        tmp.push(WRInfo::Write((Ptr::Global(x), y)));
                    }
                    WRInfo::Call(label, args) if *label == self.name.0 => {
                        //rec_call = true;
                    }
                    WRInfo::Call(label, args2) => {
                        let f = get_function_ref_opt(functions, &label);
                        match f {
                            Some(y) => {
                                let mut subst_arg = vec![];
                                for j in args2 {
                                    match j {
                                        Arg::Iter(u) => subst_arg.push(Arg::Unkown),
                                        Arg::Arg(u) => subst_arg.push(args[u].clone()),
                                        Arg::Ptr((Ptr::Arg(u), y)) => {
                                            let c = args[u].clone();
                                            match c {
                                                Arg::Ptr((x, z)) => {
                                                    let mut d = z.clone();
                                                    for i in y {
                                                        d.push(i.clone());
                                                    }
                                                    subst_arg.push((Arg::Ptr((x, d))))
                                                }
                                                _ => subst_arg.push(Arg::Unkown),
                                            }
                                        }
                                        u => subst_arg.push(u.clone()),
                                    }
                                }
                                let sub = y.collect_all_sub(functions, subst_arg);
                                for (x, y, z) in sub {
                                    // とりあえず
                                    ans.push((x, y, z));
                                }
                            }
                            None => (),
                        }
                    }
                    _ => {}
                }
            }
            ans.push((name.clone(), label.clone(), tmp.clone()));
        }
        ans
    }
    pub fn collect_writes(&self, functions: &Functions) -> Option<HashSet<String>> {
        let mut args = vec![];
        for i in 0..self.args.len() {
            args.push(Arg::Arg(i));
        }
        self.collect_writes_sub(functions, args)
    }
    pub fn collect_writes_sub(
        &self,
        functions: &Functions,
        args: Vec<Arg>,
    ) -> Option<HashSet<String>> {
        let wrinfo = self.collect_wrinfo(functions);
        let mut rec_call = false;
        let mut h = HashSet::new();
        for (name, label, writes) in wrinfo {
            for i in writes {
                match i {
                    WRInfo::Write((Ptr::Arg(idx), y)) => match args[idx].clone() {
                        Arg::Ptr((Ptr::Global(s), z)) => {
                            h.insert(s);
                        }
                        _ => {}
                    },
                    WRInfo::Write((Ptr::Global(x), y)) => {
                        h.insert(x);
                    }
                    WRInfo::Call(label, args) if *label == self.name.0 => {
                        //rec_call = true;
                        info!("label {} {:?}", label, h);
                    }
                    WRInfo::Call(label, args2) => {
                        let f = get_function_ref_opt(functions, &label);
                        match f {
                            Some(y) => {
                                let mut subst_arg = vec![];
                                for j in args2 {
                                    match j {
                                        Arg::Iter(u) => subst_arg.push(Arg::Unkown),
                                        Arg::Arg(u) => subst_arg.push(args[u].clone()),
                                        Arg::Ptr((Ptr::Arg(u), y)) => {
                                            let c = args[u].clone();
                                            match c {
                                                Arg::Ptr((x, z)) => {
                                                    let mut d = z.clone();
                                                    for i in y {
                                                        d.push(i.clone());
                                                    }
                                                    subst_arg.push((Arg::Ptr((x, d))))
                                                }
                                                _ => subst_arg.push(Arg::Unkown),
                                            }
                                        }
                                        u => subst_arg.push(u.clone()),
                                    }
                                }
                                let h2 = y.collect_writes_sub(functions, subst_arg);
                                match h2 {
                                    Some(h2) => {
                                        h2.into_iter().for_each(|x| {
                                            h.insert(x);
                                        });
                                    }
                                    None => return None,
                                }
                            }
                            None => (),
                        }
                    }
                    _ => {}
                }
            }
        }
        if rec_call && h.len() > 0 {
            None
        } else {
            Some(h)
        }
    }
    pub fn write(&self, s: &String, index: Vec<usize>, functions: &Vec<ir::Fundef>) -> bool {
        false
    }
    pub fn war_close(&self, s: &String, index: Vec<usize>, functions: &Vec<ir::Fundef>) -> bool {
        // sがwar closeか判定する
        // if の条件はGlobalのものについてだけは見る
        false
    }

    pub fn is_loop_dependent(&self, functions: &Vec<ir::Fundef>) -> bool {
        let iter = self.get_loop_idx();
        if iter.is_none() || self.has_io(functions) || (!self.only_iter_is_to_loop()) {
            // 必要条件たち
            return false;
        }
        let (iter, offset, cond) = iter.unwrap();
        // ここまで来たらあとはループの内部で変な依存関係がないことを見れば良い
        // loadオンリーな変数に対してはどうでも良い
        // storeをするグローバル変数及びargのやつを集める
        // writeするならば同一ループ内でロードするを示せば良い
        let wr = self.collect_wrinfo(functions);
        let writes = self.collect_writes(functions);
        let all = self.collect_all(functions);
        for (x, y, z) in all {
            for j in z {
                if j.is_write() {
                    //                    // もしもwriteのindexにundefがあった場合は無理
                    if j.have_undef() {
                        return false;
                    }
                    // self.war_close()
                }
            }
        }
        let graph = construct(self, functions);
        info!("{:?}", graph);
        // for i in writes {
        //    これらがaccできる or
        //     (writeされるならばloadされる ならOK)
        // }
        false
    }
    pub fn replace_self_rec_block(self) -> Self {
        let mut new_blocks = vec![];
        let mut phis = vec![];
        for mut i in self.blocks.clone() {
            //info!("{}", i.label);
            match i.get_self_rec_call(&self.name.0, &self) {
                None => new_blocks.push(i),
                Some(y) => match y {
                    ir::Inst::CallDir { dst, label, args } => {
                        info!("found loop {} ", &self.name.0);
                        phis.push((i.label.clone(), args));
                        i.inst.pop_back();
                        new_blocks.push({
                            ir::Block {
                                last: ir::ControlFlow::Jump(String::from("entry'")),
                                ..i.clone()
                            }
                        });
                    }
                    _ => unreachable!(),
                },
            }
        }
        if phis.is_empty() {
            ir::Fundef {
                blocks: new_blocks,
                ..self
            }
        } else {
            let mut entry = &mut new_blocks[ENTRY_INDEX];
            let mut newargs: Vec<_> = self
                .args
                .iter()
                .map(|(x, y)| {
                    let mut tmp = x.clone();
                    tmp.push_str(".toloop");
                    (tmp, *y)
                })
                .collect();
            let mut inst = &mut entry.inst;
            let n = self.args.len();
            let mut phis_inst = vec![vec![]; n];
            let mut phis_var = vec![HashSet::new(); n];
            let mut mask = vec![false; n];
            let mut add_phi = vec![true; n];
            for (label, args) in phis {
                for (idx, j) in args.iter().enumerate() {
                    phis_inst[idx].push((j.clone(), label.clone()));
                    match j.get_name_opt() {
                        Some(y) => {
                            phis_var[idx].insert(y);
                        }
                        None => {
                            mask[idx] = true;
                        }
                    }
                }
            }
            for idx in 0..self.args.len() {
                info!("{:?}", phis_var);
                phis_var[idx].insert(self.args[idx].0.clone());
                if phis_var[idx].len() == 1 && !mask[idx] {
                    // 結局変わってない
                    newargs[idx] = self.args[idx].clone();
                    add_phi[idx] = false;
                }
            }
            for i in 0..self.args.len() {
                if !add_phi[i] {
                    continue;
                }
                let arg = &self.args[i];
                let narg = &newargs[i];
                phis_inst[i].push((
                    knormal::Var::OpVar(narg.clone().0, narg.clone().1),
                    "entry".to_string(),
                ));
                inst.push_front(ir::Inst::Phi(ir::Phi {
                    dst: arg.clone(),
                    args: phis_inst[i].clone(),
                }));
                entry.phis.push(ir::Phi {
                    dst: arg.clone(),
                    args: phis_inst[i].clone(),
                })
            }
            ir::Fundef {
                blocks: new_blocks,
                args: newargs,
                ..self
            }
        }
    }
    pub fn get_arg_idx(&self, s: &String) -> Option<usize> {
        for (i, (x, ty)) in self.args.iter().enumerate() {
            if *x == *s {
                return Some(i);
            }
        }
        None
    }
}
#[derive(Debug)]
pub enum Wr_Cond {
    //    GlobalCmp(syntax::Cmp, String, Vec<Index>, syntax::Const),
    Other,
}

#[derive(Debug)]
pub enum Wr_ControlFlow {
    Jump(String),
    Return,
    Branch(Wr_Cond, String, String),
}

#[derive(Debug)]
pub struct Wr_Block {
    label: String,
    inst: Vec<WRInfo>,
    last: Wr_ControlFlow,
}

pub fn rename(s: &String, x: usize) -> String {
    let mut unique = s.clone();
    unique.push_str(".");
    unique.push_str(&x.to_string());
    unique
}
pub fn get_block_idx(graph: &Vec<Wr_Block>, s: &String) -> Option<usize> {
    // for i in graph{
    //     if i
    // }
    None
}
pub fn find_write_path(addr: (String, Vec<usize>), graph: &Vec<Wr_Block>, cur: usize) -> bool {
    let g = &graph[cur];
    // for i in &g.inst {
    //     match i {
    //         WRInfo::Write(wr) if global_wr_to_constant_eq(i, &addr) => {
    //             return true;
    //         }
    //         _ => (),
    //     }
    // }
    false
    // match g.last {
    //     Wr_Cond::Branch(x,y,z) => {

    //     }
    // }
}
pub fn construct(fun: &ir::Fundef, functions: &Functions) -> Vec<Wr_Block> {
    let mut blocks = vec![];
    let mut args = vec![];
    for i in 0..fun.args.len() {
        args.push(Arg::Arg(i));
    }

    construct_sub(
        fun,
        functions,
        args,
        &mut blocks,
        &mut Vec::new(),
        &mut "dummy".to_string(),
        &mut 0,
        None,
    );
    blocks
}
pub fn construct_sub(
    fun: &ir::Fundef,
    functions: &Functions,
    args: Vec<Arg>,
    blocks: &mut Vec<Wr_Block>,
    inst: &mut Vec<WRInfo>,
    label: &mut String,
    cnt: &mut usize,
    cont: Option<String>,
) {
    let num = cnt.clone();
    for block in &fun.blocks {
        let wr = fun.get_global_wr_by_block_ref(block, functions);
        let mut unique = block.label.clone();
        unique.push_str(".");
        unique.push_str(&num.to_string());
        label.clone_from(&unique);
        for i in wr {
            match i {
                WRInfo::Read((Ptr::Arg(idx), y)) => match args[idx].clone() {
                    Arg::Ptr((Ptr::Global(s), z)) => {
                        let mut tmp2 = z.clone();
                        y.iter().for_each(|x| tmp2.push(x.clone()));
                        inst.push(WRInfo::Read((Ptr::Global(s), tmp2)));
                    }
                    Arg::Ptr((Ptr::Arg(s), z)) => {
                        let mut tmp2 = z.clone();
                        y.iter().for_each(|x| tmp2.push(x.clone()));
                        inst.push(WRInfo::Read((Ptr::Arg(s), tmp2)));
                    }
                    Arg::Arg(i) => {
                        inst.push(WRInfo::Read((Ptr::Arg(i), y)));
                    }
                    _ => {
                        inst.push(WRInfo::Read((Ptr::Local, y)));
                    }
                },
                WRInfo::Read((Ptr::Global(x), y)) => {
                    inst.push(WRInfo::Read((Ptr::Global(x), y)));
                }
                WRInfo::Write((Ptr::Arg(idx), y)) => match args[idx].clone() {
                    Arg::Ptr((Ptr::Global(s), z)) => {
                        let mut tmp2 = z.clone();
                        y.iter().for_each(|x| tmp2.push(x.clone()));
                        inst.push(WRInfo::Write((Ptr::Global(s), tmp2)));
                    }
                    Arg::Ptr((Ptr::Arg(s), z)) => {
                        let mut tmp2 = z.clone();
                        y.iter().for_each(|x| tmp2.push(x.clone()));
                        inst.push(WRInfo::Write((Ptr::Arg(s), tmp2)));
                    }
                    Arg::Arg(i) => {
                        inst.push(WRInfo::Write((Ptr::Arg(i), y)));
                    }
                    _ => {
                        inst.push(WRInfo::Write((Ptr::Local, y)));
                    }
                },
                WRInfo::Write((Ptr::Global(x), y)) => {
                    inst.push(WRInfo::Write((Ptr::Global(x), y)));
                }
                WRInfo::Call(label, args) if *label == *fun.name.0 => {
                    //rec_call = true;
                }
                WRInfo::Call(l, args2) => {
                    let f = get_function_ref_opt(functions, &l);
                    match f {
                        Some(y) => {
                            let mut subst_arg = vec![];
                            for j in args2 {
                                match j {
                                    Arg::Iter(u) => subst_arg.push(Arg::Unkown),
                                    Arg::Arg(u) => subst_arg.push(args[u].clone()),
                                    Arg::Ptr((Ptr::Arg(u), y)) => {
                                        let c = args[u].clone();
                                        match c {
                                            Arg::Ptr((x, z)) => {
                                                let mut d = z.clone();
                                                for i in y {
                                                    d.push(i.clone());
                                                }
                                                subst_arg.push((Arg::Ptr((x, d))))
                                            }
                                            _ => subst_arg.push(Arg::Unkown),
                                        }
                                    }
                                    u => subst_arg.push(u.clone()),
                                }
                            }
                            let mut unique = y.name.0.clone();
                            *cnt += 1;
                            unique.push_str(".");
                            unique.push_str(&cnt.to_string());
                            blocks.push(Wr_Block {
                                label: label.to_string(),
                                inst: inst.to_vec(),
                                last: Wr_ControlFlow::Jump(unique.clone()),
                            });
                            label.clone_from(&unique);
                            unique.push_str(".cont");
                            inst.clear();
                            construct_sub(
                                y,
                                functions,
                                subst_arg,
                                blocks,
                                inst,
                                label,
                                cnt,
                                Some(unique),
                            );
                            //                            label.clone_from(unique);
                        }
                        None => (),
                    }
                }
                _ => {}
            }
        }

        blocks.push(Wr_Block {
            label: label.to_string(),
            inst: inst.to_vec(),
            last: match &block.last {
                ir::ControlFlow::Branch(x, y, z) => {
                    // let x = fun.get_instruction(x);
                    // match x {
                    //     ir::Inst::OpBinaryRR {
                    //         opcode,
                    //         dst,
                    //         lhs,
                    //         rhs
                    //     } => {
                    //         let l = fun.get_global_load(lhs,iter);
                    //         let r = fun.get_global_laod()

                    //     }
                    // }
                    Wr_ControlFlow::Branch(Wr_Cond::Other, rename(y, num), rename(z, num))
                }
                ir::ControlFlow::Return(x) if cont.clone().is_none() => Wr_ControlFlow::Return,
                ir::ControlFlow::Return(x) => Wr_ControlFlow::Jump(cont.clone().unwrap()),
                ir::ControlFlow::Jump(y) => Wr_ControlFlow::Jump(rename(y, num)),
            },
        });
        inst.clear();
        if cont.is_some() {
            label.clone_from(&cont.clone().unwrap())
        }
    }
}
