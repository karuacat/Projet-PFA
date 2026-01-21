let window_width = 800
let window_height = 600

let player_width = 24
let player_height = 24
let player_start_x = window_width / 2 - player_width / 2
let player_start_y = window_height / 2 - player_height / 2
let player_color = Texture.black
let player_v_up = Vector.{ x = 0.0; y = -5.0 }
let player_v_down = Vector.sub Vector.zero player_v_up
let player_v_right = Vector.{ x = 5.0; y = 0.0 }
let player_v_left = Vector.sub Vector.zero player_v_right

let hwall_width = window_width
let hwall_height = 0
let hwall1_x = 0
let hwall1_y = -1
let hwall2_x = 0
let hwall2_y = window_height +1
let vwall_width = 0
let vwall_height = window_height
let vwall1_x = -1
let vwall1_y = 0
let vwall2_x = window_width +1
let vwall2_y = 0