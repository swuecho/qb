(* Pure-OCaml FIPS 180-4 SHA-256, for migration checksums without extra
   dependencies. *)

let k =
  [|
    0x428a2f98l; 0x71374491l; 0xb5c0fbcfl; 0xe9b5dba5l;
    0x3956c25bl; 0x59f111f1l; 0x923f82a4l; 0xab1c5ed5l;
    0xd807aa98l; 0x12835b01l; 0x243185bel; 0x550c7dc3l;
    0x72be5d74l; 0x80deb1fel; 0x9bdc06a7l; 0xc19bf174l;
    0xe49b69c1l; 0xefbe4786l; 0x0fc19dc6l; 0x240ca1ccl;
    0x2de92c6fl; 0x4a7484aal; 0x5cb0a9dcl; 0x76f988dal;
    0x983e5152l; 0xa831c66dl; 0xb00327c8l; 0xbf597fc7l;
    0xc6e00bf3l; 0xd5a79147l; 0x06ca6351l; 0x14292967l;
    0x27b70a85l; 0x2e1b2138l; 0x4d2c6dfcl; 0x53380d13l;
    0x650a7354l; 0x766a0abbl; 0x81c2c92el; 0x92722c85l;
    0xa2bfe8a1l; 0xa81a664bl; 0xc24b8b70l; 0xc76c51a3l;
    0xd192e819l; 0xd6990624l; 0xf40e3585l; 0x106aa070l;
    0x19a4c116l; 0x1e376c08l; 0x2748774cl; 0x34b0bcb5l;
    0x391c0cb3l; 0x4ed8aa4al; 0x5b9cca4fl; 0x682e6ff3l;
    0x748f82eel; 0x78a5636fl; 0x84c87814l; 0x8cc70208l;
    0x90befffal; 0xa4506cebl; 0xbef9a3f7l; 0xc67178f2l;
  |]
  [@@ocamlformat "disable"]

let rotr x n =
  Int32.logor (Int32.shift_right_logical x n) (Int32.shift_left x (32 - n))

let ( +! ) = Int32.add
let ( ^! ) = Int32.logxor
let ( &! ) = Int32.logand

let hex s =
  let len = String.length s in
  let rem = (len + 9) mod 64 in
  let pad_len = len + 9 + if rem = 0 then 0 else 64 - rem in
  let buf = Bytes.make pad_len '\x00' in
  Bytes.blit_string s 0 buf 0 len;
  Bytes.set buf len '\x80';
  let bit_len = Int64.mul (Int64.of_int len) 8L in
  Bytes.set_int64_be buf (pad_len - 8) bit_len;
  let h0 = ref 0x6a09e667l
  and h1 = ref 0xbb67ae85l
  and h2 = ref 0x3c6ef372l
  and h3 = ref 0xa54ff53al
  and h4 = ref 0x510e527fl
  and h5 = ref 0x9b05688cl
  and h6 = ref 0x1f83d9abl
  and h7 = ref 0x5be0cd19l in
  let w = Array.make 64 0l in
  let blocks = pad_len / 64 in
  for blk = 0 to blocks - 1 do
    let base = blk * 64 in
    for i = 0 to 15 do
      w.(i) <- Bytes.get_int32_be buf (base + (i * 4))
    done;
    for i = 16 to 63 do
      let x = w.(i - 15) in
      let s0 = rotr x 7 ^! rotr x 18 ^! Int32.shift_right_logical x 3 in
      let y = w.(i - 2) in
      let s1 = rotr y 17 ^! rotr y 19 ^! Int32.shift_right_logical y 10 in
      w.(i) <- w.(i - 16) +! s0 +! w.(i - 7) +! s1
    done;
    let a = ref !h0
    and b = ref !h1
    and c = ref !h2
    and d = ref !h3
    and e = ref !h4
    and f = ref !h5
    and g = ref !h6
    and h = ref !h7 in
    for i = 0 to 63 do
      let s1 = rotr !e 6 ^! rotr !e 11 ^! rotr !e 25 in
      let ch = (!e &! !f) ^! (Int32.lognot !e &! !g) in
      let t1 = !h +! s1 +! ch +! k.(i) +! w.(i) in
      let s0 = rotr !a 2 ^! rotr !a 13 ^! rotr !a 22 in
      let maj = (!a &! !b) ^! (!a &! !c) ^! (!b &! !c) in
      let t2 = s0 +! maj in
      h := !g;
      g := !f;
      f := !e;
      e := !d +! t1;
      d := !c;
      c := !b;
      b := !a;
      a := t1 +! t2
    done;
    h0 := !h0 +! !a;
    h1 := !h1 +! !b;
    h2 := !h2 +! !c;
    h3 := !h3 +! !d;
    h4 := !h4 +! !e;
    h5 := !h5 +! !f;
    h6 := !h6 +! !g;
    h7 := !h7 +! !h
  done;
  Printf.sprintf "%08lx%08lx%08lx%08lx%08lx%08lx%08lx%08lx" !h0 !h1 !h2 !h3 !h4
    !h5 !h6 !h7
