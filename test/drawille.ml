(**
 * Monochrome canvas for drawing with Braille characters on UTF-8 terminal.
 * Inspired by Haskell and C implementations of Drawille.
 *
 *  (0, 0) pixel is in the top-left corner.
 *  x coordinate grows right.
 *  y coordinate grows down.
 * By: https://github.com/ksromanov/drawille/blob/master/lib/drawille.ml
 *)

type pixelValue = bool

let setPixel = true
let unsetPixel = false
let invertValue value = not value

type pixelCoord =
  { x : int
  ; y : int
  }

type canvas = { data : (pixelCoord, bool) Hashtbl.t }

let empty () = { data = Hashtbl.create 0 }
let create size_x size_y = { data = Hashtbl.create (size_x * size_y) }

let fromList lst =
  let canvas = empty () in
  let () =
    List.iter
      (fun (x, y) ->
        if x >= 0 && y >= 0
        then Hashtbl.replace canvas.data { x; y } setPixel
        else failwith "Negative coordinate passed to fromList")
      lst
  in
  canvas
;;

(** Dimensions of the canvas. *)
let dimensions canvas =
  let max_x, max_y =
    Hashtbl.fold (fun c _ (x, y) -> max x c.x, max y c.y) canvas.data (0, 0)
  in
  max_x + 1, max_y + 1
;;

let get canvas pixelCoord =
  try Hashtbl.find canvas.data pixelCoord with
  | Not_found -> unsetPixel
;;

let updateWithValue canvas coord value =
  if coord.x < 0 || coord.y < 0
  then failwith "Negative coordinate passed to updateWithValue"
  else if value = setPixel
  then Hashtbl.replace canvas.data coord setPixel
  else Hashtbl.remove canvas.data coord
;;

let set canvas coord = updateWithValue canvas coord setPixel
let unset canvas coord = updateWithValue canvas coord unsetPixel
let toggle canvas coord = updateWithValue canvas coord @@ invertValue @@ get canvas coord

let toDebugString canvas =
  let size_x, size_y = dimensions canvas in
  let bitmap = Bytes.make ((size_x + 1) * size_y) '?' in
  for i = 1 to size_y do
    Bytes.set bitmap ((i * (size_x + 1)) - 1) '\n'
  done;
  for row = 0 to size_y - 1 do
    for col = 0 to size_x - 1 do
      Bytes.set
        bitmap
        ((row * (size_x + 1)) + col)
        (if get canvas { x = col; y = row } then '*' else 'O')
    done
  done;
  Bytes.to_string bitmap
;;

let pixmap = [| [| 0x01; 0x08 |]; [| 0x02; 0x10 |]; [| 0x04; 0x20 |]; [| 0x40; 0x80 |] |]

let toBrailleBitmap canvas =
  let size_x, size_y = dimensions canvas in
  let b_size_x, b_size_y = (size_x + 1) / 2, (size_y + 3) / 4 in
  let bitmap = Bytes.make (b_size_x * b_size_y) '\000' in
  for row = 0 to size_y do
    let pixmapRow = pixmap.(row mod 4) in
    let bitmapRow = row / 4 in
    for col = 0 to size_x do
      let bitmapCol = col / 2 in
      let pixelWeight = pixmapRow.(col mod 2) in
      let braileIndex = (b_size_x * bitmapRow) + bitmapCol in
      if get canvas { x = col; y = row }
      then
        Bytes.set
          bitmap
          braileIndex
          (Char.chr (pixelWeight + Char.code (Bytes.get bitmap braileIndex)))
    done
  done;
  b_size_x, b_size_y, bitmap
;;

let printBrailleBitmap (_, b_size_y, bitmap) =
  let value col row = Char.code (Bytes.get bitmap ((b_size_y * row) + col)) in
  for row = 0 to b_size_y - 1 do
    for col = 0 to b_size_y - 1 do
      Printf.printf "|%x" (value col row)
    done;
    Printf.printf "|\n"
  done
;;

let frame canvas =
  let b_size_x, b_size_y, bitmap = toBrailleBitmap canvas in
  let output = Bytes.make (((3 * b_size_x) + 1) * b_size_y) '\xE2' in
  let set shift x ch = Bytes.set output (shift + x) ch in
  for row = 0 to b_size_y - 1 do
    let shift = row * ((3 * b_size_x) + 1) in
    let set x ch = set shift x ch in
    for col = 0 to b_size_x - 1 do
      let c = Char.code @@ Bytes.get bitmap ((row * b_size_x) + col) in
      let v1 =
        0xA0
        + (c land pixmap.(3).(0) / pixmap.(3).(0))
        + (c land pixmap.(3).(1) / pixmap.(3).(0))
      in
      set ((3 * col) + 1) (Char.chr v1);
      let v2 = 0xBF land c lor 0x80 in
      set ((3 * col) + 2) (Char.chr v2)
    done;
    set (3 * b_size_x) '\n'
  done;
  Bytes.to_string output
;;
