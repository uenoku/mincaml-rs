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
        let hoge = i.only_iter_is_to_loop();
        let t = i.tmp();
        if x.is_some() {
            info!("{} {:?} {} {}", i.name.0, x, hoge, i.has_io(&p));
            info!("{:?}", t);
        }
    }
    p
}
type Functions = Vec<ir::Fundef>;
pub fn get_function_ref_opt<'a>(functions: &'a Functions, s: &String) -> Option<&'a ir::Fundef> {
    for i in functions {
        if *i.name.0 == *s {
            return Some(i);
        }
    }
    None
}
#[derive(Debug)]
pub enum Index {
    Const(i32),
    Unkown,
    Iter(i32),
}
type WR = Vec<Index>;
type GlobalWR = (String, Vec<Index>);
impl ir::Inst {
    pub fn global_load(&self, fun: &ir::Fundef, iter: &String) -> Option<GlobalWR> {
        match self {
            ir::Inst::Load { dst, ptr, idx } => match fun.get_global_load(ptr, iter) {
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
                            } if (opcode == ir::OpBinaryRR::Add
                                || opcode == ir::OpBinaryRR::Sub) =>
                            {
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
                None => None,
            },
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
    pub fn tmp(&self) -> Vec<Vec<GlobalWR>> {
        let mut ans = vec![];
        for i in &self.blocks {
            ans.push(self.get_global_load_by_block_ref(i));
        }
        ans
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
                        // とりあえず
                        None
                    }
                }
            }
            knormal::Var::Ext(x, y) => Some((x.to_string(), vec![])),
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
}
