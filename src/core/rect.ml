open Vector

type t = { width : int; height : int }

type rect_f = {
  x : float;
  y : float;
  width : float;
  height : float;
}

let mdiff (v1 : Vector.t) (r1 : t) (v2 : Vector.t) (r2 : t) =
  let x = v1.x -. v2.x -. float r2.width in
  let y = v1.y -. v2.y -. float r2.height in
  let h = r1.height + r2.height in
  let w = r1.width + r2.width in
  ({ x; y }, { width = w; height = h })

let min_norm v1 v2 =
  if Vector.norm v1 <= Vector.norm v2 then v1 else v2

let has_origin (v : Vector.t) (r : t) =
  v.x < 0.0
  && v.x +. float r.width > 0.0
  && v.y < 0.0
  && v.y +. float r.height > 0.0

let intersect (v1 : Vector.t) (r1 : t) (v2 : Vector.t) (r2 : t) =
  let s_pos, s_rect = mdiff v1 r1 v2 r2 in
  has_origin s_pos s_rect

let is_zero f = f = 0.0 || f = -0.0

let penetration_vector (s_pos : Vector.t) (s_rect : t) =
  let dx_left = s_pos.x in
  let dx_right = s_pos.x +. float s_rect.width in
  let dy_top = s_pos.y in
  let dy_bottom = s_pos.y +. float s_rect.height in
  
  let abs_dx_left = abs_float dx_left in
  let abs_dx_right = abs_float dx_right in
  let abs_dy_top = abs_float dy_top in
  let abs_dy_bottom = abs_float dy_bottom in
  
  let vx =
    if abs_dx_left < abs_dx_right then
      Vector.{ x = dx_left; y = 0.0 }
    else
      Vector.{ x = dx_right; y = 0.0 }
  in
  
  let vy =
    if abs_dy_top < abs_dy_bottom then
      Vector.{ x = 0.0; y = dy_top }
    else
      Vector.{ x = 0.0; y = dy_bottom }
  in
    min_norm vx vy

let rebound (v1 : Vector.t) (r1 : t) (v2 : Vector.t) (r2 : t) =
  let overlap_x1 = (v1.x +. float r1.width) -. v2.x in
  let overlap_x2 = (v2.x +. float r2.width) -. v1.x in
  let overlap_y1 = (v1.y +. float r1.height) -. v2.y in
  let overlap_y2 = (v2.y +. float r2.height) -. v1.y in
  if overlap_x1 <= 0.0 || overlap_x2 <= 0.0 || overlap_y1 <= 0.0 || overlap_y2 <= 0.0 then
    None
  else
    let vx =
      if overlap_x1 < overlap_x2 then
        Vector.{ x = -. overlap_x1; y = 0.0 }
      else
        Vector.{ x = overlap_x2; y = 0.0 }
    in
    let vy =
      if overlap_y1 < overlap_y2 then
        Vector.{ x = 0.0; y = -. overlap_y1 }
      else
        Vector.{ x = 0.0; y = overlap_y2 }
    in
    Some (min_norm vx vy)

let collides (r1 : rect_f) (r2 : rect_f) =
  r1.x < r2.x +. r2.width &&
  r1.x +. r1.width > r2.x &&
  r1.y < r2.y +. r2.height &&
  r1.y +. r1.height > r2.y
