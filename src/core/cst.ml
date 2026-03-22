let window_width = 800
let window_height = 600

let house_width = 400
let house_height = 300
let house_offset_x = (window_width - house_width) / 2
let house_offset_y = (window_height - house_height) / 2
let house_cols = 12
let house_rows = 9
let house_scale = float_of_int house_width /. 288.0

let town_cols = 20
let town_rows = 15
let town_cell_w = window_width / town_cols
let town_cell_h = window_height / town_rows

let school_cols = 16
let school_rows = 12
let school_cell_w = window_width / school_cols
let school_cell_h = window_height / school_rows

let classroom_cols = 20
let classroom_rows = 15
let classroom_cell_w = window_width / classroom_cols
let classroom_cell_h = window_height / classroom_rows

let player_width = 24
let player_height = 20
let player_render_width = 48
let player_render_height = 48
let player_render_offset_x = -12
let player_render_offset_y = -28
let aabb_cell_size = Stdlib.max player_width player_height * 2
let debug_draw_grid = false
let player_start_x = 255
let player_start_y = 285
let player_color = Texture.black
let player_v_up = Vector.{ x = 0.0; y = -3.0 }
let player_v_down = Vector.sub Vector.zero player_v_up
let player_v_right = Vector.{ x = 3.0; y = 0.0 }
let player_v_left = Vector.sub Vector.zero player_v_right