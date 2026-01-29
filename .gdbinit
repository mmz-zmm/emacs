# --------------------------------------------------------------------------
# Pwndbg: Custom command to redirect context panes to another TTY
# This Python-based command correctly handles string arguments with special characters.
# Usage: setupwin /dev/pts/X
# --------------------------------------------------------------------------
python
import gdb

class SetupWin(gdb.Command):
    """Redirects Pwndbg context panes to a specified TTY."""
    def __init__(self):
        # Create the command "setupwin" in GDB
        super(SetupWin, self).__init__("setupwin", gdb.COMMAND_USER)

    def invoke(self, argument, from_tty):
        # This method is called when the user types "setupwin"
        if not argument:
            print("Usage: setupwin /dev/pts/X")
            return

        # The argument is passed as a single string, which we can parse safely
        tty_path = argument.strip()
        
        # List of panes you want to redirect (matches your config)
        panes_to_redirect = ['legend', 'code', 'stack', 'regs', 'disasm', 'backtrace']

        # Use gdb.execute to run commands, building the command string in Python
        gdb.execute("context off")
        
        for pane in panes_to_redirect:
            # Safely construct the command string and execute it
            gdb.execute(f"contextoutput {pane} {tty_path} true")
            
        gdb.execute("context on")
        
        print(f"Pwndbg context redirected to {tty_path}")

# Instantiate the class to register the command with GDB
SetupWin()
end


# 这两行加载配置和pwndbg本身，保持不变
#source ~/.config/gdb/gdbinit
#source /home/ywr/dsc/learn/csapp/pwndbg/gdbinit.py

set auto-load safe-path /
set print pretty on
set disassembly-flavor att
set show-compact-regs on
set context-disasm-lines 10
set context-stack-lines 10

