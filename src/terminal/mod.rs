// the parts related to raw mode are ripped from termios crate
// - extern signatures from https://github.com/dcuddeback/termios-rs/blob/master/src/ffi.rs
// - c struct from https://github.com/dcuddeback/termios-rs/blob/master/src/os/linux.rs
#![allow(non_camel_case_types)]

use std::{
    collections::VecDeque,
    io::{Read, StdinLock, StdoutLock, Write},
    os::fd::{AsRawFd, RawFd},
};

type c_int = i32;
type cc_t = u8;
type speed_t = u32;
type tcflag_t = u32;

// https://linux.die.net/man/3/tcsetattr
const OPOST: tcflag_t = 0o000001; // Enable implementation-defined output processing.
const ONLCR: tcflag_t = 0o000004; // Map NL to CR-NL on output.
const TCSANOW: c_int = 0; // the change occurs immediately.

const RIGHT: [u8; 3] = [27, 91, 67];
const LEFT: [u8; 3] = [27, 91, 68];
const BACKSPACE: u8 = 0x08;
const SPACE: u8 = 0x20;

const NCCS: usize = 32;
const HISTORY_SIZE: usize = 1024;
const PROMPT: &[u8] = b">> ";

#[derive(Debug, Copy, Clone, Eq, PartialEq, Default)]
#[repr(C)]
pub struct termios {
    c_iflag: tcflag_t,
    c_oflag: tcflag_t,
    c_cflag: tcflag_t,
    c_lflag: tcflag_t,
    c_line: cc_t,
    c_cc: [cc_t; NCCS],
    c_ispeed: speed_t,
    c_ospeed: speed_t,
}

extern "C" {
    fn cfmakeraw(termios_p: *mut termios);
    fn tcgetattr(fd: c_int, termios_p: *mut termios) -> c_int;
    fn tcsetattr(fd: c_int, optional_actions: c_int, termios_p: *const termios) -> c_int;
}

pub struct Terminal<'a> {
    stdin: StdinLock<'a>,
    stdout: StdoutLock<'a>,
    fd_in: RawFd,
    prev_cf: termios,
    history: VecDeque<Vec<char>>,
    history_offset: usize,
    buf: [u8; 1],
    cursor: usize,
}

impl Drop for Terminal<'_> {
    fn drop(&mut self) {
        unsafe {
            if tcsetattr(self.fd_in, TCSANOW, &self.prev_cf) > 0 {
                eprintln!("restore terminal failed, try typing 'reset' + Enter");
            }
        }
    }
}

impl Terminal<'_> {
    pub fn new() -> Self {
        let stdin = std::io::stdin().lock();
        let stdout = std::io::stdout().lock();
        let fd_in = stdin.as_raw_fd();
        let mut prev_cf = termios::default();
        let mut raw_cf = termios::default();
        unsafe {
            let get_errno = tcgetattr(fd_in, &mut prev_cf);
            if get_errno > 0 {
                panic!("getting terminal attr failed with errno {get_errno}")
            }

            cfmakeraw(&mut raw_cf);
            raw_cf.c_oflag |= ONLCR | OPOST;

            let set_errno = tcsetattr(fd_in, TCSANOW, &raw_cf);
            if set_errno > 0 {
                panic!("setting terminal to raw failed with errno {set_errno}")
            }
        }
        Self {
            stdin,
            stdout,
            fd_in,
            prev_cf,
            history: VecDeque::with_capacity(HISTORY_SIZE),
            history_offset: 0,
            buf: [0],
            cursor: 0,
        }
    }

    fn read_char(&mut self) -> Option<char> {
        let mut tmp = vec![];
        loop {
            self.stdin
                .read_exact(&mut self.buf)
                .expect("reading byte failed");
            tmp.push(self.buf[0]);
            if tmp.len() > 4 {
                return None;
            }
            if let Ok(c) = std::str::from_utf8(&tmp) {
                return c.chars().next();
            }
        }
    }

    pub fn flush(&mut self) {
        self.stdout.flush().expect("stdout flush failed");
    }

    pub fn write(&mut self, bytes: &[u8]) {
        self.stdout.write_all(bytes).expect("stdout write failed");
    }

    pub fn writeln(&mut self, bytes: &[u8]) {
        self.write(bytes);
        self.write(b"\n");
    }

    fn remove_char(&mut self, input: &mut Vec<char>) {
        input.remove(self.cursor);
        self.update_output_from_cursor(input, input.len() + 1);
    }

    fn move_word_left(&mut self, input: &[char]) {
        let moves = match input[..self.cursor]
            .iter()
            .rev()
            .position(|c| !c.is_whitespace())
        {
            None => self.cursor, // no word found, go to prompt
            Some(word) => match input[..self.cursor - word]
                .iter()
                .rev()
                .position(|c| c.is_whitespace())
            {
                None => self.cursor,         // no whitespace found, go to prompt
                Some(space) => word + space, // go to whitespace
            },
        };
        if moves > 0 {
            self.write(format!("\x1b[{moves}D").as_bytes());
            self.cursor -= moves;
        }
    }

    fn move_to_start(&mut self) {
        if self.cursor > 0 {
            self.write(format!("\x1b[{}D", self.cursor).as_bytes());
            self.cursor = 0;
        }
    }

    fn move_to_end(&mut self, input: &[char]) {
        if self.cursor < input.len() {
            self.write(format!("\x1b[{}C", input.len() - self.cursor).as_bytes());
            self.cursor = input.len();
        }
    }

    fn remove_word_left(&mut self, input: &mut Vec<char>) {
        let moves = match input[..self.cursor]
            .iter()
            .rev()
            .position(|c| !c.is_whitespace())
        {
            None => self.cursor, // no word found, go to prompt
            Some(word) => match input[..self.cursor - word]
                .iter()
                .rev()
                .position(|c| c.is_whitespace())
            {
                None => self.cursor,         // no whitespace found, go to prompt
                Some(space) => word + space, // go to whitespace
            },
        };
        if moves > 0 {
            let prev_len = input.len();
            input.drain(self.cursor - moves..self.cursor);
            self.write(format!("\x1b[{moves}D").as_bytes());
            self.cursor -= moves;
            self.update_output_from_cursor(input, prev_len);
        }
    }

    fn move_word_right(&mut self, input: &[char]) {
        let moves = match input[self.cursor..].iter().position(|c| !c.is_whitespace()) {
            None => input.len() - self.cursor, // no word found, go to end
            Some(word) => match input[self.cursor + word..]
                .iter()
                .position(|c| c.is_whitespace())
            {
                None => input.len() - self.cursor, // no whitespace found, go to end
                Some(space) => word + space,       // go to whitespace
            },
        };
        if moves > 0 {
            self.write(format!("\x1b[{moves}C").as_bytes());
            self.cursor += moves;
        }
    }

    fn remove_word_right(&mut self, input: &mut Vec<char>) {
        let moves = match input[self.cursor..].iter().position(|c| !c.is_whitespace()) {
            None => input.len() - self.cursor, // no word found, go to end
            Some(word) => match input[self.cursor + word..]
                .iter()
                .position(|c| c.is_whitespace())
            {
                None => input.len() - self.cursor, // no whitespace found, go to end
                Some(space) => word + space,       // go to whitespace
            },
        };
        if moves > 0 {
            let prev_len = input.len();
            input.drain(self.cursor..self.cursor + moves);
            self.update_output_from_cursor(input, prev_len);
        }
    }

    fn update_output_from_cursor(&mut self, input: &[char], prev_len: usize) {
        let output = input[self.cursor..].iter().collect::<String>();
        self.write(output.as_bytes());
        if prev_len > input.len() {
            self.write(b"\x1b[0K");
        }
        let moves = input.len() - self.cursor;
        if moves > 0 {
            self.write(format!("\x1b[{moves}D").as_bytes());
        }
    }

    fn replace_output(&mut self, input: &[char], prev_len: usize) {
        self.write(b"\r");
        self.write(PROMPT);
        if prev_len > input.len() {
            self.write(&vec![SPACE; prev_len]); // clear prev text
            self.write(&vec![BACKSPACE; prev_len]); // go to prompt
        }
        let output = input.iter().collect::<String>();
        self.write(output.as_bytes());
    }

    fn prev_history(&mut self, input: &mut Vec<char>) {
        if self.history_offset == self.history.len() {
            return;
        }
        let prev_len = input.len();
        self.history_offset += 1;
        let idx = self.history.len() - self.history_offset;
        *input = self.history[idx].clone();
        self.replace_output(input, prev_len);
        self.cursor = input.len();
    }

    fn next_history(&mut self, input: &mut Vec<char>) {
        let prev_len = input.len();
        if self.history_offset > 0 {
            self.history_offset -= 1;
        }
        if self.history_offset == 0 {
            input.clear();
            self.replace_output(input, prev_len);
            self.cursor = 0;
            return;
        }
        let idx = self.history.len() - self.history_offset;
        *input = self.history[idx].clone();
        self.replace_output(input, prev_len);
        self.cursor = input.len();
    }

    fn handle_esc_sequence(&mut self, input: &mut Vec<char>) {
        match self.read_char() {
            Some('[') => match self.read_char() {
                // up
                Some('A') => self.prev_history(input),
                // down
                Some('B') => self.next_history(input),
                // left
                Some('D') => {
                    if self.cursor > 0 {
                        self.cursor -= 1;
                        self.write(&LEFT);
                    }
                }
                // right
                Some('C') => {
                    if self.cursor < input.len() {
                        self.cursor += 1;
                        self.write(&RIGHT);
                    }
                }
                Some('1') => match self.read_char() {
                    // Home
                    Some('~') => self.move_to_start(),
                    Some(';') => {
                        match self.read_char() {
                            // SHIFT
                            Some('2') => _ = self.read_char(),
                            // ALT
                            Some('3') => match self.read_char() {
                                Some('D') => self.move_word_left(input),
                                Some('C') => self.move_word_right(input),
                                _ => (),
                            },
                            // CTRL
                            Some('5') => match self.read_char() {
                                Some('D') => self.move_word_left(input),
                                Some('C') => self.move_word_right(input),
                                _ => (),
                            },
                            _ => (),
                        }
                    }
                    _ => (),
                },
                Some('3') => match self.read_char() {
                    // delete
                    Some('~') => {
                        if self.cursor < input.len() {
                            self.remove_char(input);
                        }
                    }
                    Some(';') => match self.read_char() {
                        // SHIFT
                        Some('2') => _ = self.read_char(),
                        // ALT
                        Some('3') => {
                            if let Some('~') = self.read_char() {
                                self.remove_word_right(input);
                            }
                        }
                        // CTRL
                        Some('5') => {
                            if let Some('~') = self.read_char() {
                                self.remove_word_right(input);
                            }
                        }
                        _ => (),
                    },
                    _ => (),
                },
                Some('4') => {
                    // End
                    if let Some('~') = self.read_char() {
                        self.move_to_end(input)
                    }
                }
                _ => (),
            },
            // alt-del
            Some('\u{7f}') => self.remove_word_left(input),
            _ => (),
        }
    }

    pub fn get_input(&mut self) -> Option<String> {
        let mut input: Vec<char> = vec![];
        self.cursor = 0;

        self.write(PROMPT);
        self.flush();

        while let Some(c) = self.read_char() {
            match c {
                // ctrl-c
                '\u{3}' => break,
                // newline
                '\r' => {
                    self.write(b"\n");
                    if input.iter().filter(|c| !c.is_whitespace()).count() == 0 {
                        self.write(PROMPT);
                        self.flush();
                        continue;
                    }

                    self.history_offset = 0;
                    self.history.push_back(input.clone());
                    if self.history.len() > HISTORY_SIZE {
                        self.history.pop_front();
                    }

                    if input.iter().filter(|c| !c.is_whitespace()).last() != Some(&';') {
                        input.push(';')
                    }
                    return Some(input.into_iter().collect());
                }
                // backspace
                '\u{7f}' => {
                    if self.cursor > 0 {
                        self.cursor -= 1;
                        self.write(&LEFT);
                        self.remove_char(&mut input);
                    }
                }
                // ctrl-a
                '\u{1}' => self.move_to_start(),
                // ctrl-e
                '\u{5}' => self.move_to_end(&input),
                // ctrl-backspace
                '\u{8}' => self.remove_word_left(&mut input),
                // ctrl-q
                '\u{11}' => (),
                // ctrl-k
                '\u{b}' => {
                    self.write(b"\x1b[0K");
                    input.drain(self.cursor..);
                }
                // ctrl-u
                '\u{15}' => {
                    if self.cursor > 0 {
                        let prev_len = input.len();
                        input.drain(..self.cursor);
                        self.write(format!("\x1b[{}D", self.cursor).as_bytes());
                        self.cursor = 0;
                        self.update_output_from_cursor(&input, prev_len);
                    }
                }
                // ctrl-w
                '\u{17}' => self.remove_word_left(&mut input),
                // start of ESC sequence
                '\u{1b}' => self.handle_esc_sequence(&mut input),
                c => {
                    input.insert(self.cursor, c);
                    self.update_output_from_cursor(&input, input.len() - 1);
                    self.cursor += 1;
                    self.write(&RIGHT);
                }
            }
            self.flush();
        }
        None
    }
}
