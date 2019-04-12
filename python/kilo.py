# -*- coding: utf-8 -*-
import sys
import tty
import copy
import curses.ascii
import termios
import contextlib


### terminal ###

@contextlib.contextmanager
def raw_mode(fileno):
    old_attr = termios.tcgetattr(fileno)

    new_attr = copy.deepcopy(old_attr)
    new_attr[tty.IFLAG] &= ~(termios.BRKINT | termios.ICRNL | termios.IXON
                             | termios.INPCK | termios.ISTRIP)
    new_attr[tty.OFLAG] &= ~(termios.OPOST)
    new_attr[tty.CFLAG] |= (termios.CS8)
    new_attr[tty.LFLAG] &= ~(termios.ECHO | termios.ICANON | termios.IEXTEN
                             | termios.ISIG)
    # set the number of characters read at a time in non-canonical mode.
    new_attr[tty.CC][termios.VMIN] = 1
    # set timeout for read
    new_attr[tty.CC][termios.VTIME] = 1

    termios.tcsetattr(fileno, termios.TCSAFLUSH, new_attr)

    yield

    termios.tcsetattr(fileno, termios.TCSAFLUSH, old_attr)


def main():
    while True:
        c = sys.stdin.read(1)
        if c == 'q':
            return
        elif curses.ascii.iscntrl(c):
            print(f'{ord(c)}', end='\r\n')
        else:
            print(f"{ord(c)} ('{c}')", end='\r\n')


if __name__ == '__main__':
    with raw_mode(sys.stdin.fileno()):
        main()
