open Vector

type t = { width : int; height : int }

let mdiff v1 r1 v2 r2 =
  let x = v1.x -. v2.x -. float r2.width in
  let y = v1.y -. v2.y -. float r2.height in
  let h = r1.height + r2.height in
  let w = r1.width + r2.width in
  ({ x; y }, { width = w; height = h })

let min_norm v1 v2 =
  if Vector.norm v1 <= Vector.norm v2 then v1 else v2

let has_origin v r =
  v.x < 0.0
  && v.x +. float r.width > 0.0
  && v.y < 0.0
  && v.y +. float r.height > 0.0

let intersect v1 r1 v2 r2 =
  let s_pos, s_rect = mdiff v1 r1 v2 r2 in
  has_origin s_pos s_rect

let is_zero f = f = 0.0 || f = -0.0

let penetration_vector s_pos s_rect =
  let dx_left = s_pos.x in
  let dx_right = s_pos.x +. float s_rect.width in
  let dy_top = s_pos.y in
  let dy_bottom = s_pos.y +. float s_rect.height in
  let vx =
    if abs_float dx_left < abs_float dx_right then
      Vector.{ x = dx_left; y = 0.0 }
    else
      Vector.{ x = dx_right; y = 0.0 }
    in
    let vy =
      if abs_float dy_top < abs_float dy_bottom then
        Vector.{ x = 0.0; y = dy_top }
      else
        Vector.{ x = 0.0; y = dy_bottom }
    in
    min_norm vx vy

let rebound v1 r1 v2 r2 = 
  let s_pos, s_rect = mdiff v1 r1 v2 r2 in
  if has_origin s_pos s_rect then
    Some (penetration_vector s_pos s_rect)
  else None