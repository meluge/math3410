let a = Char.code 'A'

let is_AZ c =
  let x = Char.code c in
  x >= Char.code 'A' && x <= Char.code 'Z'

let mod26 x =
  let r = x mod 26 in
  if r < 0 then r + 26 else r

let shift_upper c k =
  Char.chr (a + mod26 (Char.code c - a + k))

let progressive_shift_decrypt (s : string) : string =
  let buf = Buffer.create (String.length s) in
  let rec go i pos =
    if pos = String.length s then ()
    else
      let c = s.[pos] in
      if is_AZ c then (
        Buffer.add_char buf (shift_upper c (-i));
        go (i + 1) (pos + 1)
      ) else (
        Buffer.add_char buf c;
        go i (pos + 1)
      )
  in
  go 1 0;
  Buffer.contents buf

let vigenere_decrypt ~(keyword : string) (ciphertext : string) : string =
  let key = String.uppercase_ascii keyword in
  let keylen = String.length key in
  let key_shift j =
    let kc = key.[j mod keylen] in
    Char.code kc - a
  in
  let buf = Buffer.create (String.length ciphertext) in
  let rec go j pos =
    if pos = String.length ciphertext then ()
    else
      let c = ciphertext.[pos] in
      if is_AZ c then (
        let k = key_shift j in
        Buffer.add_char buf (shift_upper c (-k));
        go (j + 1) (pos + 1)
      ) else (
        Buffer.add_char buf c;
        go j (pos + 1)
      )
  in
  go 0 0;
  Buffer.contents buf

let () =
  let c1 = "XJHRFTNZHMZGAHIUETXZJNBWNUTRHEPOMDNBJMAUGORFAOIZOCC" in
  Printf.printf "Exercise 1.1(c):\n%s\n\n" (progressive_shift_decrypt c1);

  let c2 =
"KHFEQ YMSCI ETCSI GJVPW FFBSQ
MOAPX ZCSFX EPSOX YENPK DAICX
CEBSM TTPTX ZOOEQ LAFLG KIPOC
ZSWQM TAUJW GHBOH VRJTQ HU"
  in
  Printf.printf "Exercise 5.11(b):\n%s\n"
    (vigenere_decrypt ~keyword:"rabbithole" c2)
