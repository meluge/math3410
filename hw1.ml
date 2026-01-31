open Printf


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
  let ciphertext = String.uppercase_ascii ciphertext in  
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




let ciphertext_517 =
"togmg gbymk kcqiv dmlxk kbyif vcuek cuuis vvxqs pwwej koqgg
phumt whlsf yovww knhhm rcqfq vvhkw psued ugrsf ctwij khvfa
thkef fwptj ggviv cgdra pgwvm osqxg hkdvt whuev kcwyj psgsn
gfwsl jsfse ooqhw tofsh aciin gfbif gabgj adwsy topml ecqzw
asgvs fwrqs fsfvq rhdrs nmvmk cbhrv kblxk gzi"

let mic_rows : ((int * int) * float array) list =
  [
    ((1,2), [|
      0.044;0.047;0.021;0.054;0.046;0.038;0.022;0.034;0.057;0.035;0.040;0.023;0.038;
      0.040;0.063;0.033;0.025;0.032;0.055;0.038;0.030;0.032;0.045;0.035;0.030;0.044
    |]);
    ((1,3), [|
      0.038;0.031;0.027;0.037;0.045;0.036;0.034;0.032;0.039;0.039;0.047;0.038;0.050;
      0.026;0.046;0.042;0.053;0.027;0.024;0.040;0.047;0.048;0.018;0.037;0.034;0.066
    |]);
    ((1,4), [|
      0.025;0.039;0.053;0.043;0.023;0.035;0.032;0.043;0.029;0.040;0.041;0.050;0.027;
      0.042;0.050;0.042;0.031;0.024;0.052;0.027;0.051;0.020;0.037;0.042;0.069;0.031
    |]);
    ((1,5), [|
      0.050;0.050;0.025;0.031;0.038;0.045;0.037;0.028;0.032;0.038;0.063;0.033;0.034;
      0.030;0.048;0.039;0.030;0.034;0.038;0.042;0.035;0.036;0.043;0.055;0.030;0.035
    |]);
    ((2,3), [|
      0.035;0.037;0.039;0.031;0.031;0.035;0.047;0.048;0.034;0.031;0.031;0.067;0.053;
      0.039;0.015;0.030;0.045;0.049;0.037;0.023;0.036;0.030;0.049;0.039;0.050;0.037
    |]);
    ((2,4), [|
      0.040;0.033;0.046;0.031;0.033;0.023;0.052;0.027;0.031;0.039;0.078;0.034;0.029;
      0.027;0.048;0.050;0.037;0.032;0.021;0.035;0.043;0.047;0.041;0.047;0.042;0.035
    |]);
    ((2,5), [|
      0.042;0.040;0.042;0.029;0.033;0.035;0.035;0.038;0.037;0.057;0.039;0.038;0.040;
      0.033;0.035;0.039;0.033;0.037;0.047;0.037;0.028;0.034;0.066;0.054;0.032;0.022
    |]);
    ((3,4), [|
      0.032;0.033;0.035;0.049;0.053;0.027;0.030;0.022;0.047;0.036;0.040;0.036;0.052;
      0.040;0.048;0.041;0.044;0.033;0.028;0.039;0.027;0.036;0.017;0.038;0.051;0.065
    |]);
    ((3,5), [|
      0.043;0.043;0.040;0.034;0.033;0.034;0.043;0.035;0.026;0.030;0.050;0.068;0.044;
      0.039;0.029;0.045;0.040;0.033;0.028;0.031;0.037;0.038;0.036;0.033;0.051;0.036
    |]);
    ((4,5), [|
      0.045;0.033;0.044;0.046;0.021;0.032;0.030;0.038;0.047;0.040;0.025;0.037;0.068;
      0.049;0.033;0.029;0.043;0.028;0.033;0.020;0.040;0.040;0.041;0.039;0.039;0.059
    |]);
  ]

let mic_row i j =
  let x,y = if i < j then (i,j) else (j,i) in
  List.assoc (x,y) mic_rows

let shifts_ge row thr =
  let acc = ref [] in
  Array.iteri (fun s v -> if v >= thr then acc := (s,v) :: !acc) row;
  List.sort (fun (_,a) (_,b) -> compare b a) !acc

let rotate_keyword (kw:string) (s:int) : string =
  let kw = String.uppercase_ascii kw in
  let b = Bytes.of_string kw in
  for i = 0 to Bytes.length b - 1 do
    let c = Bytes.get b i in
    if is_AZ c then Bytes.set b i (shift_upper c s)
  done;
  Bytes.to_string b

let english_freq =
  [|
    0.08167;0.01492;0.02782;0.04253;0.12702;0.02228;0.02015;0.06094;0.06966;
    0.00153;0.00772;0.04025;0.02406;0.06749;0.07507;0.01929;0.00095;0.05987;
    0.06327;0.09056;0.02758;0.00978;0.02360;0.00150;0.01974;0.00074
  |]

let chi2_english (s:string) =
  let s = String.uppercase_ascii s in
  let counts = Array.make 26 0 in
  let n = ref 0 in
  String.iter (fun c ->
    if is_AZ c then (counts.(Char.code c - a) <- counts.(Char.code c - a) + 1; incr n)
  ) s;
  if !n = 0 then infinity else
  let nf = float_of_int !n in
  let acc = ref 0.0 in
  for i = 0 to 25 do
    let expected = english_freq.(i) *. nf in
    if expected > 0.0 then
      let o = float_of_int counts.(i) in
      let d = o -. expected in
      acc := !acc +. (d *. d /. expected)
  done;
  !acc

let () =
  let thr =
    if Array.length Sys.argv >= 2 then float_of_string Sys.argv.(1) else 0.060
  in

  Printf.printf "\nExercise 5.17 (hard-coded Table 5.15), threshold=%.3f\n\n" thr;

  Printf.printf "Shifts with MIC >= %.3f for each pair (i,j):\n" thr;
  List.iter (fun (i,j) ->
    let big = shifts_ge (mic_row i j) thr in
    if big <> [] then begin
      Printf.printf "  (%d,%d):" i j;
      List.iter (fun (s,v) -> Printf.printf "  s=%2d:%.3f" s v) big;
      print_newline ()
    end
  ) pairs;


  let base_key = "AMBCQ" in
  Printf.printf "\nBase key (k1=0): %s\n" base_key;
  Printf.printf "Trying all 26 rotations and ranking by English chi^2...\n\n";

  let scored = ref [] in
  for rot = 0 to 25 do
    let kw = rotate_keyword base_key rot in
    let pt = vigenere_decrypt ~keyword:kw ciphertext_517 in
    let score = chi2_english pt in
    scored := (score, kw, pt) :: !scored
  done;
  let scored = List.sort (fun (a,_,_) (b,_,_) -> compare a b) !scored in

  let rec take n xs =
    match n, xs with
    | 0, _ | _, [] -> []
    | n, x::tl -> x :: take (n-1) tl
  in

  List.iteri (fun idx (score, kw, pt) ->
    let snippet =
      if String.length pt > 120 then String.sub pt 0 120 ^ "..."
      else pt
    in
    Printf.printf "#%d keyword=%s  chi2=%.2f\n%s\n\n" (idx+1) kw score snippet
  ) (take 3 scored);

  let pt_codes = vigenere_decrypt ~keyword:"CODES" ciphertext_517 in
  Printf.printf "Decryption with keyword=CODES:\n%s\n" pt_codes
