#load "str.cma";;

let remove_diacritics str =
  let translation = [("ã", "a"); ("é", "e")] in
  let rec remove str map =
    match map with
      | [] -> str
      | (character, replacement)::t ->
        remove (Str.global_replace (Str.regexp_string character) replacement str) t
        in
  remove str translation
  ;;

let () =
  print_string (remove_diacritics "éã!");
  print_string "\n"
  ;;
