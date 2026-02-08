let window_width = 800
let window_height = 600

let player_width = 24
let player_height = 24
let aabb_cell_size = Stdlib.max player_width player_height * 2
let debug_draw_grid = false
let player_start_x = window_width / 2 - player_width / 2
let player_start_y = window_height / 2 - player_height / 2
let player_color = Texture.black
let player_v_up = Vector.{ x = 0.0; y = -3.0 }
let player_v_down = Vector.sub Vector.zero player_v_up
let player_v_right = Vector.{ x = 3.0; y = 0.0 }
let player_v_left = Vector.sub Vector.zero player_v_right