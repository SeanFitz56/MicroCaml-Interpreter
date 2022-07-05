open TokenTypes

let re_num = Str.regexp "[0-9]+\\|(-[0-9]+)"
let rpen = Str.regexp ")"
let lpen = Str.regexp "("
let eq = Str.regexp "="
let noteq = Str.regexp "<>"
let greater = Str.regexp ">"
let less = Str.regexp "<"
let greater_eq = Str.regexp ">="
let lesser_eq = Str.regexp "<="
let re_or = Str.regexp "||"
let re_and = Str.regexp "&&"
let re_not = Str.regexp "not"
let re_if = Str.regexp "if"
let re_then = Str.regexp "then"
let re_else = Str.regexp "else"
let re_add = Str.regexp "\\+"
let re_sub = Str.regexp "-"
let re_mul = Str.regexp "\\*"
let re_div = Str.regexp "/"
let re_concat = Str.regexp "\\^"
let re_let = Str.regexp "let"
let re_def = Str.regexp "def"
let re_in = Str.regexp "in"
let re_rec = Str.regexp "rec"
let re_fun = Str.regexp "fun"
let arrow = Str.regexp "->"
let dob_semi = Str.regexp ";;"
let bool = Str.regexp "true\\|false"
let re_str = Str.regexp "\"[^\"]*\""
let re_id = Str.regexp "[a-zA-Z][a-zA-Z0-9]*"
let white_space = Str.regexp " "
let tabs = Str.regexp "\t"
let new_line = Str.regexp "\n"

let rec sanitize str_lst = 
    match str_lst with 
    [] -> []
    |h::t -> if h = '\"' then sanitize t else h::(sanitize t)

let rec build_str str_lst =
    match str_lst with 
    [] -> ""
    |h::t -> (String.make 1 h)^(build_str t)

let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []


let negi_to_i str =
    let rec helper lst =
        match lst with 
        [] -> []
        |h::t -> if h = '(' || h = ')' then (helper t) else h::(helper t)
    in build_str (helper (explode str))


let tokenize input = 
    let rec tok pos s =
    if pos >= (String.length s) then []
    else if (Str.string_match re_num s pos) then 
        let str = (Str.matched_string s) in let npos = Str.match_end() in (Tok_Int (int_of_string (negi_to_i str)))::(tok (npos) s)
    else if (Str.string_match rpen s pos) then 
        let npos = Str.match_end()  in Tok_RParen::(tok (npos) s)
    else if (Str.string_match lpen s pos) then 
        let npos = Str.match_end() in  Tok_LParen::(tok (npos) s)
    else if (Str.string_match arrow s pos) then 
        let npos = Str.match_end() in Tok_Arrow::(tok (npos) s)
    else if (Str.string_match greater_eq s pos) then 
        let npos = Str.match_end() in Tok_GreaterEqual::(tok (npos) s)
    else if (Str.string_match lesser_eq s pos) then 
        let npos = Str.match_end() in Tok_LessEqual::(tok (npos) s)
    else if (Str.string_match noteq s pos) then 
        let npos = Str.match_end() in Tok_NotEqual::(tok (npos) s)
    else if (Str.string_match eq s pos) then 
        let npos = Str.match_end() in Tok_Equal::(tok (npos) s)
    else if (Str.string_match greater s pos) then 
        let npos = Str.match_end() in Tok_Greater::(tok (npos) s)
    else if (Str.string_match less s pos) then 
        let npos = Str.match_end() in Tok_Less::(tok (npos) s)
    else if (Str.string_match re_or s pos) then 
        let npos = Str.match_end() in Tok_Or::(tok (npos) s)
    else if (Str.string_match re_and s pos) then 
        let npos = Str.match_end() in Tok_And::(tok (npos) s)
    else if (Str.string_match re_not s pos) then 
        let npos = Str.match_end() in Tok_Not::(tok (npos) s)
    else if (Str.string_match re_if s pos) then 
        let npos = Str.match_end() in Tok_If::(tok (npos) s)
    else if (Str.string_match re_then s pos) then 
        let npos = Str.match_end() in Tok_Then::(tok (npos) s)
    else if (Str.string_match re_else s pos) then 
        let npos = Str.match_end() in Tok_Else::(tok (npos) s)
    else if (Str.string_match re_add s pos) then 
        let npos = Str.match_end() in Tok_Add::(tok (npos) s)
    else if (Str.string_match re_sub s pos) then 
        let npos = Str.match_end() in Tok_Sub::(tok (npos) s)
    else if (Str.string_match re_div s pos) then 
        let npos = Str.match_end() in Tok_Div::(tok (npos) s)
    else if (Str.string_match re_mul s pos) then 
        let npos = Str.match_end() in Tok_Mult::(tok (npos) s)
    else if (Str.string_match re_concat s pos) then 
        let npos = Str.match_end() in Tok_Concat::(tok (npos) s)
    else if (Str.string_match re_let s pos) then 
        let npos = Str.match_end() in Tok_Let::(tok (npos) s)
    else if (Str.string_match re_def s pos) then 
        let npos = Str.match_end() in Tok_Def::(tok (npos) s)
    else if (Str.string_match re_in s pos) then 
        let npos = Str.match_end() in Tok_In::(tok (npos) s)
    else if (Str.string_match re_rec s pos) then 
        let npos = Str.match_end() in Tok_Rec::(tok (npos) s)
    else if (Str.string_match re_fun s pos) then 
        let npos = Str.match_end() in Tok_Fun::(tok (npos) s)
    else if (Str.string_match dob_semi s pos) then 
        let npos = Str.match_end() in Tok_DoubleSemi::(tok (npos) s)
    else if (Str.string_match bool s pos) then 
        let str = Str.matched_string s in 
        let npos = Str.match_end() in Tok_Bool (if String.equal str "true" then true else false)::(tok (npos) s)
    else if (Str.string_match re_id s pos) then 
        let str = Str.matched_string s in 
        let npos = Str.match_end() in
        (Tok_ID str)::(tok (npos) s)
    else if (Str.string_match re_str s pos) then 
        let str = Str.matched_string s in 
        let npos = Str.match_end() in
        Tok_String (build_str (sanitize (explode str)))::(tok (npos) s)
    else if (Str.string_match white_space s pos) then 
        let npos = Str.match_end() in
        tok (npos) s
    else if (Str.string_match tabs s pos) then 
        let npos = Str.match_end() in
        tok (npos) s
    else if (Str.string_match new_line s pos) then 
     let npos = Str.match_end() in
        tok (npos) s
    else raise (InvalidInputException("lexer"))

    in tok 0 (input)