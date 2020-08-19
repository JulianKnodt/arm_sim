use std::convert::TryInto;

#[derive(Debug)]
pub struct CPU {
  registers: Registers,
  mem: Vec<u8>,

  // TODO mark endianness
}

const NUM_GENERAL: usize = 31;
const NUM_FLOATING: usize = 32;
#[derive(Debug, Default)]
pub struct Registers {
  general: [u64; NUM_GENERAL],

  floating_point: [u128; NUM_FLOATING],

  program_counter: u64,
  // stack_pointers: []
}

const ZERO_FULL_WIDTH: u64 = 0;
const ZERO_HALF_WIDTH: u32 = 0;

static mut IGNORE_ZERO_FULL_WIDTH: u64 = 0;
static mut IGNORE_ZERO_HALF_WIDTH: u32 = 0;

macro_rules! full_width_registers {
  ($( $reg_name: ident, $backing_idx: expr $(,)? )* ) => {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum FullRegisterIndex { $( $reg_name, )* XZR, LR, }
    impl std::ops::Index<FullRegisterIndex> for Registers {
      type Output = u64;
      fn index(&self, i: FullRegisterIndex) -> &Self::Output {
        match i {
          $( FullRegisterIndex::$reg_name => &self.general[$backing_idx], )*
          FullRegisterIndex::XZR => &ZERO_FULL_WIDTH,
          FullRegisterIndex::LR => &self.general[30],
        }
      }
    }

    impl std::ops::IndexMut<FullRegisterIndex> for Registers {
      fn index_mut(&mut self, i: FullRegisterIndex) -> &mut Self::Output {
        match i {
          $( FullRegisterIndex::$reg_name => &mut self.general[$backing_idx], )*
          FullRegisterIndex::XZR => unsafe { &mut IGNORE_ZERO_FULL_WIDTH },
          FullRegisterIndex::LR => &mut self.general[30],
        }
      }
    }
  }
}

macro_rules! half_width_registers {
  ($( $reg_name: ident, $backing_idx: expr $(,)? )* ) => {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum HalfRegisterIndex { $( $reg_name, )* WZR, }
    impl std::ops::Index<HalfRegisterIndex> for Registers {
      type Output = u32;
      fn index(&self, i: HalfRegisterIndex) -> &Self::Output {
        match i {
          $(
            HalfRegisterIndex::$reg_name => unsafe {
              // TODO see if this is ok
              &*(&self.general[$backing_idx] as *const _ as *const u32)
            },
          )*
          HalfRegisterIndex::WZR => &ZERO_HALF_WIDTH,
        }
      }
    }

    impl std::ops::IndexMut<HalfRegisterIndex> for Registers {
      fn index_mut(&mut self, i: HalfRegisterIndex) -> &mut Self::Output {
        match i {
          $(
            // TODO see if this is ok
            HalfRegisterIndex::$reg_name => unsafe {
              &mut *(&mut self.general[$backing_idx] as *mut _ as *mut u32)
            },
          )*
          HalfRegisterIndex::WZR => unsafe { &mut IGNORE_ZERO_HALF_WIDTH },
        }
      }
    }

  }
}

full_width_registers!(
  X0, 0, X1, 1, X2, 2, X3, 3, X4, 4, X5, 5, X6, 6, X7, 7, X8, 8, X9, 9, X10, 10, X11, 11, X12, 12,
  X13, 13, X14, 14, X15, 15, X16, 16, X17, 17, X18, 18, X19, 19, X20, 20, X21, 21, X22, 22, X23,
  23, X24, 24, X25, 25, X26, 26, X27, 27, X28, 28, X29, 29, X30, 30,
);

half_width_registers!(
  W0, 0, W1, 1, W2, 2, W3, 3, W4, 4, W5, 5, W6, 6, W7, 7, W8, 8, W9, 9, W10, 10, W11, 11, W12, 12,
  W13, 13, W14, 14, W15, 15, W16, 16, W17, 17, W18, 18, W19, 19, W20, 20, W21, 21, W22, 22, W23,
  23, W24, 24, W25, 25, W26, 26, W27, 27, W28, 28, W29, 29, W30, 30,
);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum RegisterIndex {
  FullWidth(FullRegisterIndex),
  HalfWidth(HalfRegisterIndex),
}

macro_rules! full_fp_registers {
  ($( $reg_name: ident, $backing_idx: expr $(,)? )* ) => {
    #[derive(Debug, Copy, Clone, PartialEq, Eq)]
    pub enum FullFPRegisterIndex {
      $( $reg_name, )*
    }
    impl std::ops::Index<FullFPRegisterIndex> for Registers {
      type Output = u128;
      fn index(&self, i: FullFPRegisterIndex) -> &Self::Output {
        match i {
          $( FullFPRegisterIndex::$reg_name => &self.floating_point[$backing_idx], )*
        }
      }
    }

    impl std::ops::IndexMut<FullFPRegisterIndex> for Registers {
      fn index_mut(&mut self, i: FullFPRegisterIndex) -> &mut Self::Output {
        match i {
          $( FullFPRegisterIndex::$reg_name => &mut self.floating_point[$backing_idx], )*
        }
      }
    }
  }
}

full_fp_registers!(
  V0, 0, V1, 1, V2, 2, V3, 3, V4, 4, V5, 5, V6, 6, V7, 7, V8, 8, V9, 9, V10, 10, V11, 11, V12, 12,
  V13, 13, V14, 14, V15, 15, V16, 16, V17, 17, V18, 18, V19, 19, V20, 20, V21, 21, V22, 22, V23,
  23, V24, 24, V25, 25, V26, 26, V27, 27, V28, 28, V29, 29, V30, 30, V31, 31,
);


#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum LitOrReg {
  Lit(u64),
  Reg(RegisterIndex),
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum IndexingMode {
  None,
  PreIndexing,
  PostIndexing,
}

/// Represents either a register or an immediate.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum MARKER {
  Literal(u64),
  GeneralRegister(RegisterIndex),
  MemAccess(RegisterIndex, LitOrReg, IndexingMode)
}
impl CPU {
  // Resolves either an immediate or a register value
  fn read_from_reg(&self, r: RegisterIndex) -> u64 {
    match r {
      RegisterIndex::FullWidth(f) => self.registers[f],
      RegisterIndex::HalfWidth(h) => self.registers[h] as u64,
    }
  }
  fn write_to_reg(&mut self, r: RegisterIndex, v: u64) {
    match r {
      RegisterIndex::FullWidth(f) => self.registers[f] = v,
      RegisterIndex::HalfWidth(h) => self.registers[h] = v as u32,
    }
  }
  pub fn resolve(&self, m: MARKER) -> u64 { todo!() }
  // Loads from memory and also possibly modifies the register.
  // TODO check width loaded here
  pub fn load(&mut self, m: MARKER) -> u64 {
    if let MARKER::MemAccess(reg, l_or_r, idx_mode) = m {
      let base = self.read_from_reg(reg);
      let offset = match l_or_r {
        LitOrReg::Lit(l) => l,
        LitOrReg::Reg(r) => self.read_from_reg(r),
      };
      let idx_native = base + offset;
      let idx = idx_native as usize;
      let read_from = match idx_mode {
        IndexingMode::None => idx,
        IndexingMode::PreIndexing => {
          self.write_to_reg(reg, idx_native);
          idx
        },
        IndexingMode::PostIndexing => base as usize,
      };
      let v = u64::from_le_bytes(self.mem[read_from..read_from+8].try_into().unwrap());
      if let IndexingMode::PostIndexing = idx_mode {
          self.write_to_reg(reg, idx_native);
      }
      v
    } else {
      panic!("Did not receive memory address access: {:?}", m);
    }
  }
  pub fn set_reg(&mut self, m: MARKER, v: u64) { todo!() }
  pub fn read_reg(&self, m: MARKER) -> u64 { todo!() }
  pub fn store<const BYTES: usize>(&mut self, _m: MARKER, _addr: u64) {
    todo!()
  }
  pub fn branch(&mut self, _addr: u64) {
    todo!()
  }
  pub fn resolve_label(&self, label: MARKER) -> u64 {
    todo!()
  }
}

macro_rules! define_alu_ops {
  ($self: ident, $( $operand: ident($len: expr) = |$ops: pat| $body: expr $(,)? )+) => {
    #[derive(Clone, PartialEq, Eq)]
    pub enum Operand { $( $operand([MARKER; $len]), )+ }

    impl CPU {
      /// Applies this operation to the CPU
      pub fn apply_op(&mut $self, op: &Operand) {
        match op {
          $( &Operand::$operand($ops) => $body, )+
        }
      }
    }

  }
}

define_alu_ops!(
  // necessary to have this self for use in functions
  self,
  ADD(3) = |[dest, op1, op2]| self.set_reg(dest, self.read_reg(op1) + self.resolve(op2)),
  SUB(3) = |[dest, op1, op2]| self.set_reg(dest, self.read_reg(op1) - self.resolve(op2)),
  MOV(2) = |[dest, src]| self.set_reg(dest, self.resolve(src)),
  MVN(2) = |[dest, src]| self.set_reg(dest, !self.resolve(src)),
  LDR(2) = |[dest, addr]| {
    let v = self.load(addr);
    self.set_reg(dest, v)
  },
  /*
  STRW(2) = |[src, addr]| self.store::<4>(src, self.load(addr)),
  STRH(2) = |[src, addr]| self.store::<2>(src, self.load(addr)),
  STRB(2) = |[src, addr]| self.store::<1>(src, self.load(addr)),
  */

  B(1) = |[dst]| self.branch(self.resolve_label(dst)),
);
