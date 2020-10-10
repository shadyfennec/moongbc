use crate::opcode::Opcode;
use crate::register::Reg16;
use crate::{cpu::CPU, memory_map::Interconnect};
use std::collections::VecDeque;

/// A window of instructions, mainly used by the disassembler
/// in order to access to several instructions, alongside their
/// arguments and their address in memory.
pub struct InstructionWindow {
    pub(crate) instructions: Vec<Line>,
    pub(crate) cursor: usize,
    pub(crate) pc: Option<usize>,
}

impl InstructionWindow {
    /// Creates an `InstructionWindow` at address `cursor`, with `before` instructions before
    /// and `after` instructions after. If there are not enough instructions to accomodate for
    /// the `before` and `after` instructions, it will add more instructions to either of the
    /// ends so that the number of instructions is always `before + after`.
    pub fn build(
        before: usize,
        after: usize,
        cursor: u16,
        cpu: &CPU,
        memory: &Interconnect,
    ) -> InstructionWindow {
        let disassembler = Disassembler::new();

        let instructions = disassembler.find_window(cpu, memory, before, after, cursor);

        let cursor = instructions
            .iter()
            .enumerate()
            .find(|(_, (a, _))| *a == cursor)
            .map(|(i, _)| i)
            .unwrap_or(0);

        let pc = instructions
            .iter()
            .enumerate()
            .find(|(_, (a, _))| *a == Reg16::PC.read(&cpu.registers))
            .map(|(i, _)| i);

        InstructionWindow {
            instructions,
            cursor,
            pc,
        }
    }
}

struct Queue<T> {
    inner: VecDeque<T>,
    size: usize,
}

impl<T> Queue<T> {
    pub fn new(size: usize) -> Queue<T> {
        Queue {
            inner: VecDeque::new(),
            size,
        }
    }

    pub fn push(&mut self, element: T) -> Option<T> {
        let result = if self.inner.len() >= self.size {
            self.inner.pop_front()
        } else {
            None
        };

        self.inner.push_back(element);

        result
    }

    #[allow(clippy::wrong_self_convention)]
    pub fn to_vec(self) -> Vec<T> {
        self.inner.into()
    }

    pub fn is_full(&self) -> bool {
        self.inner.len() == self.size
    }
}

type Line = (u16, Opcode);

/// A helper struct, used to disassemble instructions from the memory without actually
/// modifying the PC register or jumping around with the emulated CPU.
pub struct Disassembler {}

impl Disassembler {
    #[allow(clippy::new_without_default)]
    /// Creates a new `Disassembler`.
    pub fn new() -> Disassembler {
        Disassembler {}
    }

    fn find_window(
        &self,
        cpu: &CPU,
        memory: &Interconnect,
        before: usize,
        after: usize,
        target: u16,
    ) -> Vec<Line> {
        let mut pc = 0;
        let mut queue = Queue::new(before + after + 1);

        let mut found = false;
        let mut counter = after + 1;

        for opcode in cpu.preview_iterator(0, memory) {
            if found {
                counter = counter.saturating_sub(1);
            }

            if pc == target {
                found = true;
            }

            if queue.is_full() && found && counter == 0 {
                break;
            }

            //println!("0x{:04x}", pc);

            let size = opcode.size();
            queue.push((pc, opcode));
            pc += size as u16;
        }

        queue.to_vec()
    }

    /// Returns whether or not the specified address is a valid
    /// address, assuming the alignments starts at 0.
    pub fn check_address(&self, cpu: &CPU, memory: &Interconnect, address: u16) -> bool {
        let mut pointer = 0;

        for opcode in cpu.preview_iterator(0, memory) {
            if pointer >= address {
                break;
            }

            pointer += opcode.size() as u16;
        }

        pointer == address
    }

    /// Returns the size of the instruction located at the specified address,
    /// and the one before it. If the specified address is 0, it returns instead
    /// the size of the first and second instruction.
    pub fn surrounding_sizes(&self, cpu: &CPU, memory: &Interconnect, pc: u16) -> (usize, usize) {
        let result = self
            .find_window(cpu, memory, 1, 0, pc)
            .iter()
            .map(|(_, o)| o.size())
            .collect::<Vec<_>>();

        assert_eq!(result.len(), 2);

        if pc == 0 {
            (0, result[0])
        } else {
            (result[0], result[1])
        }
    }
}
