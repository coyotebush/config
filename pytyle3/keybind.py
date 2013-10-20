import state
import tile

bindings = {
    'Mod4-a':           tile.cmd('tile'),
    'Mod4-Shift-a':     tile.cmd('untile'),
    'Mod4-h':           tile.cmd('decrease_master'),
    'Mod4-l':           tile.cmd('increase_master'),
    #'Mod4-j':          tile.cmd('prev_client'),
    #'Mod4-k':          tile.cmd('next_client'),
    'Mod4-Shift-j':     tile.cmd('switch_prev_client'),
    'Mod4-Shift-k':     tile.cmd('switch_next_client'),
    'Mod4-comma':       tile.cmd('add_master'),
    'Mod4-period':      tile.cmd('remove_master'),
    'Mod4-Control-Return': tile.cmd('make_master'),
    'Mod4-m':           tile.cmd('focus_master'),
    'Mod4-z':           tile.cmd('cycle'),

    #'Mod4-q':       tile.debug_state,
    #'Mod4-Shift-q': state.quit,
}

