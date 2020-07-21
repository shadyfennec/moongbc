use crate::cpu::CPU;
use crate::opcode::Opcode;
use crate::register::Reg16;
use crate::Src;
use std::collections::VecDeque;

pub struct InstructionWindow {
    pub(crate) instructions: Vec<Line>,
    pub(crate) cursor: usize,
    pub(crate) pc: Option<usize>,
}

impl InstructionWindow {
    pub fn build(before: usize, after: usize, cursor: u16, cpu: &CPU) -> InstructionWindow {
        let disassembler = Disassembler::new();

        let instructions = disassembler.find_window(cpu, before, after, cursor);

        let cursor = instructions
            .iter()
            .enumerate()
            .find(|(_, (a, _))| *a == cursor)
            .map(|(i, _)| i)
            .unwrap_or(0);

        let pc = instructions
            .iter()
            .enumerate()
            .find(|(_, (a, _))| *a == Reg16::PC.read(cpu))
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

pub struct Disassembler {}

impl Disassembler {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Disassembler {
        Disassembler {}
    }

    fn find_window(&self, cpu: &CPU, before: usize, after: usize, target: u16) -> Vec<Line> {
        let mut pc = 0;
        let mut queue = Queue::new(before + after + 1);

        let mut found = false;
        let mut counter = after + 1;

        for opcode in cpu.preview_iterator(0) {
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

    pub fn check_address(&self, cpu: &CPU, address: u16) -> bool {
        let mut pointer = 0;

        for opcode in cpu.preview_iterator(0) {
            if pointer >= address {
                break;
            }

            pointer += opcode.size() as u16;
        }

        pointer == address
    }

    pub fn surrounding_sizes(&self, cpu: &CPU, pc: u16) -> (usize, usize) {
        let result = self
            .find_window(cpu, 1, 0, pc)
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
