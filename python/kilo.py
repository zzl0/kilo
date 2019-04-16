# -*- coding: utf-8 -*-
import os
import sys
import tty
import time
import copy
import fcntl
import struct
from curses.ascii import iscntrl
from enum import Enum, auto
import termios
import contextlib

import logging
logging.basicConfig(filename='kilo.log',level=logging.DEBUG)

KILO_VERSION = '0.0.1'
KILO_QUIT_TIMES = 2
HL_HIGHLIGHT_NUMBERS = 1 << 0
HL_HIGHLIGHT_STRINGS = 1 << 1


class Key(Enum):
    ESC = 27
    BACKSPACE = 127
    ARROW_LEFT = 1000
    ARROW_RIGHT = auto()
    ARROW_UP = auto()
    ARROW_DOWN = auto()
    DEL = auto()
    HOME = auto()
    END = auto()
    PAGE_UP = auto()
    PAGE_DOWN = auto()


SEQ_2_KEY = {
    b'[1~': Key.HOME,
    b'[4~': Key.END,
    b'[3~': Key.DEL,
    b'[5~': Key.PAGE_UP,
    b'[6~': Key.PAGE_DOWN,
    b'[7~': Key.HOME,
    b'[8~': Key.END,
    b'[A': Key.ARROW_UP,
    b'[B': Key.ARROW_DOWN,
    b'[C': Key.ARROW_RIGHT,
    b'[D': Key.ARROW_LEFT,
    b'[H': Key.HOME,
    b'[F': Key.END,
}


class Highlight(Enum):
    NORMAL = 37
    COMMENT = 36
    KEYWORD1 = 33
    KEYWORD2 = 32
    STRING = 35
    NUMBER = 31
    MATCH = 34


class FileSyntax:
    def __init__(self, filetype, filematch, keywords, singleline_comment_start, flags):
        self.filetype = filetype
        self.filematch = filematch
        self.keywords = keywords
        self.singleline_comment_start = singleline_comment_start
        self.flags = flags

HLDB = [
    FileSyntax(
        "Python",
        ['.py'],
        [
            # Python keywords
            "and", "as", "assert", "break", "class", "continue", "def", "del", "elif", "else", "except", "exec", "finally", "for", "from", "global", "if", "import", "in", "is", "lambda", "not", "or", "pass", "print", "raise", "return", "try", "while", "with", "yield",
            # Python types
            "buffer|", "bytearray|", "complex|", "False|", "float|", "frozenset|", "int|", "list|", "long|", "None|", "set|", "str|", "tuple|", "True|", "type|", "unicode|", "xrange|"
        ],
        '#',
        HL_HIGHLIGHT_NUMBERS | HL_HIGHLIGHT_STRINGS
    ),
]

### terminal

@contextlib.contextmanager
def raw_mode(fd):
    old_attr = termios.tcgetattr(fd)

    new_attr = copy.deepcopy(old_attr)
    new_attr[tty.IFLAG] &= ~(termios.BRKINT | termios.ICRNL | termios.IXON
                             | termios.INPCK | termios.ISTRIP)
    new_attr[tty.OFLAG] &= ~(termios.OPOST)
    new_attr[tty.CFLAG] |= (termios.CS8)
    new_attr[tty.LFLAG] &= ~(termios.ECHO | termios.ICANON | termios.IEXTEN
                             | termios.ISIG)
    # set the number of characters read at a time in non-canonical mode.
    new_attr[tty.CC][termios.VMIN] = 0
    # set timeout for read
    new_attr[tty.CC][termios.VTIME] = 1

    termios.tcsetattr(fd, termios.TCSAFLUSH, new_attr)

    try:
        yield
    finally:
        termios.tcsetattr(fd, termios.TCSAFLUSH, old_attr)


def get_window_size(fd):
    return struct.unpack('hh', fcntl.ioctl(fd, termios.TIOCGWINSZ, '1234'))


def read_key(fd):
    while True:
        c = os.read(fd, 1)
        if c:
            break

    d = ord(c)
    if d == Key.ESC.value:
        seq = os.read(fd, 3)
        if len(seq) < 2:
            return Key.ESC
        if seq in SEQ_2_KEY:
            return SEQ_2_KEY[seq]
        return Key.ESC
    else:
        try:
            return Key(d)
        except ValueError:
            return d


def is_separator(c):
    return c.isspace() or c == '\0' or c in ',.()+-/*=~%<>[];:'


def ctrl(c):
    return ord(c) & 0x1f

### append buffer

class AppendBuffer():
    def __init__(self):
        self.buf = b''

    def append(self, s):
        if isinstance(s, str):
            self.buf += s.encode()
        else:
            self.buf += s

### Row

class Row():
    KILO_TAB_STOP = 8

    def __init__(self, chars, editor):
        self.editor = editor
        self.set_chars(chars)

    def set_chars(self, chars):
        self.chars = chars
        self.render = self.gen_render_chars()
        self.update_syntax()

    @property
    def size(self):
        return len(self.chars)

    @property
    def rsize(self):
        return len(self.render)

    def gen_render_chars(self):
        lst = []
        for c in self.chars:
            if c == '\t':
                size = len(lst)
                spaces = self.KILO_TAB_STOP - (size % self.KILO_TAB_STOP)
                lst.extend([' '] * spaces)
            else:
                lst.append(c)
        return ''.join(lst)

    def insert_char(self, at, c):
        self.set_chars(self.chars[:at] + c + self.chars[at:])

    def del_char(self, at):
        self.set_chars(self.chars[:at] + self.chars[at + 1:])

    def append_str(self, s):
        self.set_chars(self.chars + s)

    def cx2rx(self, cx):
        rx = 0
        for i in range(cx):
            if self.chars[i] == '\t':
                rx += (self.KILO_TAB_STOP - 1) - (rx % self.KILO_TAB_STOP)
            rx += 1
        return rx

    def rx2cx(self, rx):
        cur_rx = 0
        for cx, c in enumerate(self.chars):
            if c == '\t':
                cur_rx += (self.KILO_TAB_STOP - 1) - (curr_rx % self.KILO_TAB_STOP)
            cur_rx += 1
            if cur_rx > rx:
                return cx
        return cx

    def update_syntax(self):
        self.hl = [Highlight.NORMAL] * self.rsize
        syntax = self.editor.syntax
        if not syntax:
            return

        keywords = self.editor.syntax.keywords
        scs = self.editor.syntax.singleline_comment_start

        prev_sep = True
        in_string = None

        i = 0
        while i < self.rsize:
            c = self.render[i]
            prev_hl = self.hl[i - 1] if i > 0 else Highlight.NORMAL

            if scs and not in_string:
                if self.render[i: i + len(scs)] == scs:
                    for j in range(i, self.rsize):
                        self.hl[j] = Highlight.COMMENT
                    break

            if syntax.flags & HL_HIGHLIGHT_STRINGS:
                if in_string:
                    self.hl[i] = Highlight.STRING
                    if c == '\\' and i + 1 < self.rsize:
                        self.hl[i + 1] = Highlight.STRING
                        i += 2
                        continue
                    if c == in_string:
                        in_string = None
                    i += 1
                    prev_sep = True
                    continue
                elif c == '"' or c == "'":
                    in_string = c
                    self.hl[i] = Highlight.STRING
                    i += 1
                    continue

            if syntax.flags & HL_HIGHLIGHT_NUMBERS:
                if (c.isdigit() and (prev_sep or prev_hl == Highlight.NUMBER)) \
                    or (c == '.' and prev_hl == Highlight.NUMBER):
                    self.hl[i] = Highlight.NUMBER
                    i += 1
                    prev_sep = False
                    continue

            if prev_sep:
                found = False
                for kw in keywords:
                    is_kw2 = kw[-1] == '|'
                    if is_kw2:
                        kw = kw[:-1]

                    end = i + len(kw)
                    if self.render[i: end] == kw and (end == self.rsize or is_separator(self.render[end])):
                        found = True
                        for j in range(i, end):
                            self.hl[j] = Highlight.KEYWORD2 if is_kw2 else Highlight.KEYWORD1
                        i = end
                        break
                if found:
                    prev_sep = False
                    continue

            prev_sep = is_separator(c)
            i += 1

### Editor

class Editor():
    def __init__(self):
        self.in_fd = sys.stdin.fileno()
        self.out_fd = sys.stdout.fileno()
        self.filename = None

        self.cx = 0
        self.cy = 0
        self.rx = 0
        self.screenrows, self.screencols = get_window_size(self.out_fd)
        self.screenrows -= 2  # reserved last two rows for status bar and message
        self.rowoff = 0
        self.coloff = 0
        self.rows = []
        self.dirty = 0
        self.statusmsg = ''
        self.statusmsg_time = None

        self.last_match = -1
        self.direction = 1
        self.saved_hl_line = None
        self.saved_hl = None
        self.syntax = None

    @property
    def numrows(self):
        return len(self.rows)

    def open(self, filename):
        self.filename = filename
        self.select_syntax()

        with open(filename) as f:
            for line in f:
                line = line.rstrip('\r\n')
                self.insert_row(self.numrows, line)
        self.dirty = 0

    def select_syntax(self):
        if not self.filename:
            return

        try:
            ext = '.' + self.filename.rsplit('.', 1)[1]
        except IndexError:
            ext = None

        for syntax in HLDB:
            for m in syntax.filematch:
                is_ext = m[0] == '.'
                if ((is_ext and ext and ext == m) or (not is_ext and self.filename == m)):
                    self.syntax = syntax
                    return

    def insert_row(self, at, s):
        if 0 <= at <= self.numrows:
            self.rows.insert(at, Row(s, self))
            self.dirty += 1

    def del_row(self, at):
        if 0 <= at < self.numrows:
            self.rows.pop(at)
            self.dirty += 1

    def insert_char(self, c):
        if self.cy == self.numrows:
            self.insert_row(0, "")
        self.rows[self.cy].insert_char(self.cx, c)
        self.cx += 1
        self.dirty += 1

    def insert_newline(self):
        if self.cx == 0:
            self.insert_row(self.cy, "")
        else:
            row = self.rows[self.cy]
            self.insert_row(self.cy + 1, row.chars[self.cx:])
            row.set_chars(row.chars[:self.cx])
        self.cy += 1
        self.cx = 0

    def del_char(self):
        if (self.cx == 0 and self.cy == 0) or self.cy == self.numrows:
            return
        row = self.rows[self.cy]
        if self.cx > 0:
            row.del_char(self.cx - 1)
            self.cx -= 1
        else:
            pre_row = self.rows[self.cy - 1]
            self.cx = pre_row.size
            pre_row.append_str(row.chars)
            self.del_row(self.cy)
            self.cy -= 1
        self.dirty += 1

    ## output

    def scroll(self):
        self.rx = self.rows[self.cy].cx2rx(self.cx) if self.cy < self.numrows else 0

        if self.cy < self.rowoff:
            self.rowoff = self.cy
        elif self.cy >= self.rowoff + self.screenrows:
            self.rowoff = self.cy - self.screenrows + 1

        if self.rx < self.coloff:
            self.coloff = self.rx
        elif self.rx >= self.coloff + self.screencols:
            self.coloff = self.rx - self.screencols + 1

    def draw_rows(self, ab):
        for y in range(self.screenrows):
            filerow = y + self.rowoff
            if filerow >= self.numrows:
                if self.numrows == 0 and y == self.screenrows // 3:
                    welcome_msg = f'Kilo editor -- version {KILO_VERSION}'.encode()
                    welcome_len = min(len(welcome_msg), self.screencols)
                    padding = (self.screencols - welcome_len) // 2
                    if padding:
                        ab.append(b'~' + b' ' * padding)
                    ab.append(welcome_msg[:welcome_len])
                else:
                    ab.append(b'~')
            else:
                curr_color = -1
                row = self.rows[filerow]
                s = row.render[self.coloff: self.coloff + self.screencols]
                for i, c in enumerate(s):
                    j = i + self.coloff
                    color = row.hl[j].value
                    if color != curr_color:
                        curr_color = color
                        ab.append(f'\x1b[{color}m')
                    ab.append(c)
                ab.append(f'\x1b[{Highlight.NORMAL.value}m')

            # erases the part of the line to the right of the cursor
            ab.append(b'\x1b[K')
            ab.append(b'\r\n')

    def draw_status_bar(self, ab):
        ab.append(b'\x1b[7m')
        filename = self.filename or "No Name"
        dirtymsg = '(modified)' if self.dirty else ''
        status = f'{filename} {self.numrows} lines {dirtymsg}'
        ab.append(status)
        filetype = self.syntax.filetype if self.syntax else 'no ft'
        rstatus = f'{filetype} | {self.cy + 1}/{self.numrows}'
        for _ in range(len(status), self.screencols - len(rstatus)):
            ab.append(b' ')
        ab.append(rstatus)
        ab.append(b'\x1b[m')
        ab.append(b'\r\n')

    def draw_message_bar(self, ab):
        ab.append(b'\x1b[K')
        if time.time() - self.statusmsg_time < 5:
            ab.append(self.statusmsg)

    def refresh_screen(self):
        self.scroll()
        ab = AppendBuffer()

        # hide cursor
        ab.append(b'\x1b[?25l')
        # reposition the cursor
        ab.append(b'\x1b[H')

        self.draw_rows(ab)
        self.draw_status_bar(ab)
        self.draw_message_bar(ab)

        ab.append(f'\x1b[{self.cy - self.rowoff + 1};{self.rx - self.coloff + 1}H')
        # show cursor
        ab.append(b'\x1b[?25h')

        os.write(self.out_fd, ab.buf)

    def set_status_message(self, format, *args):
        self.statusmsg = format % args
        self.statusmsg_time = time.time()

    ## input

    def prompt(self, s, callback=lambda buf, c: None):
        buf = ''
        while True:
            self.set_status_message(s, buf)
            self.refresh_screen()

            c = read_key(self.in_fd)
            if c in (Key.DEL, ctrl('h'), Key.BACKSPACE):
                if buf:
                    buf = buf[:-1]
            elif c == Key.ESC:
                self.set_status_message('')
                callback(buf, c)
                return ''
            elif c == ord('\r'):
                if buf:
                    self.set_status_message('')
                    callback(buf, c)
                    return buf
            elif isinstance(c, int) and not iscntrl(c) and c < 128:
                buf += chr(c)

            callback(buf, c)

    def process_keypress(self):
        k = read_key(self.in_fd)

        if k == ord('\r'):
            self.insert_newline()
        elif k == ctrl('q'):
            if self.dirty:
                if not hasattr(self, 'quit_times'):
                    self.quit_times = KILO_QUIT_TIMES
                if self.quit_times > 0:
                    self.set_status_message(
                        'WARNING!!! File has unsaved changes.'
                        f'Press Ctrl-Q {self.quit_times} more times to quite.'
                    )
                    self.quit_times -= 1
                    return
            os.write(self.out_fd, b'\x1b[2J')
            os.write(self.out_fd, b'\x1b[H')
            sys.exit(0)
        elif k == ctrl('s'):
            self.save()
        elif k == Key.HOME:
            self.cx = 0
        elif k == Key.END:
            if self.cy < self.numrows:
                self.cx = self.rows[self.cy].size
        elif k == ctrl('f'):
            self.find()
        elif k in (Key.BACKSPACE, ctrl('h'), Key.DEL):
            if k == Key.DEL:
                self.move_cursor(Key.ARROW_RIGHT)
            self.del_char()
        elif k in (Key.PAGE_DOWN, Key.PAGE_UP):
            if k == Key.PAGE_UP:
                self.cy = self.rowoff
            else:
                self.cy = min(self.numrows, self.rowoff + self.screenrows - 1)
            for _ in range(self.screenrows):
                arrow_key = Key.ARROW_UP if k == Key.PAGE_UP else Key.ARROW_DOWN
                self.move_cursor(arrow_key)
        elif k in (Key.ARROW_DOWN, Key.ARROW_LEFT, Key.ARROW_RIGHT, Key.ARROW_UP):
            self.move_cursor(k)
        elif k in (ctrl('l'), Key.ESC):
            # do nothing
            pass
        else:
            self.insert_char(chr(k))

        self.quit_times = KILO_QUIT_TIMES

    def move_cursor(self, key):
        if key == Key.ARROW_LEFT:
            if self.cx != 0:
                self.cx -= 1
            elif self.cy > 0:
                self.cy -= 1
                self.cx = self.rows[self.cy].size
        elif key == Key.ARROW_RIGHT:
            try:
                if self.cx < self.rows[self.cy].size:
                    self.cx += 1
                elif self.cx == self.rows[self.cy].size:
                    self.cy += 1
                    self.cx = 0
            except IndexError:
                pass
        elif key == key.ARROW_UP:
            if self.cy != 0:
                self.cy -= 1
        elif key == Key.ARROW_DOWN:
            if self.cy < self.numrows:
                self.cy += 1

        try:
            row = self.rows[self.cy]
            self.cx = min(self.cx, row.size)
        except IndexError:
            self.cx = 0

    def rows2str(self):
        return '\n'.join(r.chars for r in self.rows)

    def save(self):
        if not self.filename:
            self.filename = self.prompt("Save as: %s (ESC to cancel)")
            if not self.filename:
                self.set_status_message('Save aborted')
                return

            self.select_syntax()
            for row in self.rows:
                row.update_syntax()

        with open(self.filename, 'w') as f:
            s = self.rows2str()
            f.write(s)
            self.dirty = 0
            self.set_status_message(f"{len(s)} bytes written to disk")

    ## find

    def find_callback(self, query, key):
        if self.saved_hl:
            row = self.rows[self.saved_hl_line]
            row.hl = self.saved_hl

        if key in (b'\r', Key.ESC):
            self.last_match = -1
            self.direction = 1
            return
        elif key in (Key.ARROW_RIGHT, Key.ARROW_DOWN):
            self.direction = 1
        elif key in (Key.ARROW_LEFT, Key.ARROW_UP):
            self.direction = -1
        else:
            self.last_match = -1
            self.direction = 1

        current = self.last_match
        for i in range(self.numrows):
            current += self.direction
            if current == -1:
                current = self.numrows - 1
            elif current == self.numrows:
                current = 0
            row = self.rows[current]
            idx = row.render.find(query)
            if idx != -1:
                self.last_match = current
                self.cy = current
                self.cx = row.rx2cx(idx)
                self.rowoff = self.numrows

                self.saved_hl = row.hl[:]
                self.saved_hl_line = current
                for i in range(idx, idx + len(query)):
                    row.hl[i] = Highlight.MATCH
                break

    def find(self):
        cx, cy, coloff, rowoff = self.cx, self.cy, self.coloff, self.rowoff
        query = self.prompt('Search: %s (Use ESC/Arrows/Enter)', self.find_callback)
        if not query:
            self.cx, self.cy, self.coloff, self.rowoff = cx, cy, coloff, rowoff

    ## run

    def run(self, filename):
        if filename:
            self.open(filename)
        self.set_status_message("HELP: Ctrl-S = save | Ctrl-Q = quit | Ctrl-F = find")
        while True:
            self.refresh_screen()
            self.process_keypress()


if __name__ == '__main__':
    with raw_mode(sys.stdin.fileno()):
        editor = Editor()
        filename = sys.argv[1] if len(sys.argv) > 1 else ''
        editor.run(filename)
