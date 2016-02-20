#load "str.cma";;

let remove_diacritics str =
  let table = [
    ("a", ["ã"; "á"; "à"; "â"]);
    ("e", ["é"; "è"; "ê"]);
    ("i", ["í"; "ì"; "î"]);
    ("o", ["ó"; "ò"; "ô"]);
    ("u", ["ú"; "ù"]);
    ("ñ", ["n"])
  ] in
  let rec replace str replacement characters = match characters with
    | [] -> str
    | character :: tail ->
      replace
        (Str.global_replace (Str.regexp_string character) replacement str)
        replacement
        tail
  in
  let rec remove str map =
    match map with
      | [] -> str
      | (_, [])::_ -> str
      | (replacement, characters)::t ->
        remove (replace str replacement characters) t
      in
  remove str table
  ;;

let () =
  print_string (remove_diacritics "Olá, como você vai?");
  print_string "\n"
  ;;
