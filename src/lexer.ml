open Types

(* Part 1: Lexer *)

let grab_token reg input = 
    let ans = Re.Group.get (Re.exec reg input) 1 in
    let ans_len = String.length ans in
    (ans,ans_len)

let make_reg input = Re.compile (Re.Perl.re ("^(" ^ input ^ ")"))

let reduce_input input offset = (String.sub input offset ((String.length input) - offset))

let rec tokenize input = 
    let whitespace  = make_reg "[ \\t\\n]+" in
    let bool_re     = make_reg "true|false" in
    let int_re_pos  = make_reg "[0-9]+"in
    let int_re_neg  = Re.compile (Re.Perl.re "^\\((-[0-9]*)\\)") in
    let reg_lparen  = make_reg "\\(" in
    let reg_rparen  = make_reg "\\)" in
    let reg_lcurly  = make_reg "\\{" in
    let reg_rcurly  = make_reg "\\}" in
    let reg_dot     = make_reg "\\." in
    let reg_equal   = make_reg "=" in
    let reg_notequal= make_reg "<>" in
    let reg_geq     = make_reg ">=" in
    let reg_leq     = make_reg "<=" in
    let reg_less    = make_reg "<" in
    let reg_greater = make_reg ">" in
    let reg_or      = make_reg "\\|\\|" in
    let reg_and     = make_reg "&&" in
    let reg_not     = make_reg "not" in
    let reg_if      = make_reg "if" in
    let reg_then    = make_reg "then" in
    let reg_else    = make_reg "else" in
    let reg_add     = make_reg "\\+" in
    let reg_sub     = make_reg "-" in
    let reg_mult    = make_reg "\\*" in
    let reg_div     = make_reg "/" in
    let reg_concat  = make_reg "\\^" in
    let reg_let     = make_reg "let" in
    let reg_def     = make_reg "def" in
    let reg_in      = make_reg "in" in
    let reg_rec     = make_reg "rec" in
    let reg_fun     = make_reg "fun" in
    let reg_arrow   = make_reg "->" in
    let reg_dubsemi = make_reg ";;" in
    let reg_semi    = make_reg ";" in
    let reg_string  = Re.compile (Re.Perl.re "^\\\"([^\\\"]*)\\\"") in
    let reg_id_5    = make_reg "[a-zA-Z][a-zA-Z0-9]{4}[a-zA-Z0-9]*" in
    let reg_id_4    = make_reg "[a-zA-Z][a-zA-Z0-9]{3}[a-zA-Z0-9]*" in
    let reg_id_3    = make_reg "[a-zA-Z][a-zA-Z0-9]{2}[a-zA-Z0-9]*" in
    let reg_id      = make_reg "[a-zA-Z][a-zA-Z0-9]*" in
    
    
    if input = ""
        then []
else if Re.execp whitespace input then
    let (ans,len) = grab_token whitespace input in
    tokenize (String.sub input len ((String.length input) - len))
else if Re.execp bool_re input then
    let (ans,len) = grab_token bool_re input in
    Tok_Bool(ans="true")::(tokenize (String.sub input len ((String.length input) - len)))
else if Re.execp int_re_pos input then
    let (ans,len) = grab_token int_re_pos input in
    Tok_Int(int_of_string ans)::(tokenize (String.sub input len ((String.length input) - len)))
else if Re.execp int_re_neg input then
    let (ans,len) = grab_token int_re_neg input in
    let len = len + 2 in
    Tok_Int(int_of_string ans)::(tokenize (String.sub input len ((String.length input) - len)))
else if Re.execp reg_string input then
    let (ans,len) = grab_token reg_string input in
    let len = len + 2 in
    Tok_String(ans)::(tokenize (String.sub input len ((String.length input) - len)))
else if Re.execp reg_lparen input then
    Tok_LParen :: (tokenize (reduce_input input 1))
else if Re.execp reg_rparen input then
    Tok_RParen :: (tokenize (reduce_input input 1))
else if Re.execp reg_lcurly input then
    Tok_LCurly :: (tokenize (reduce_input input 1))
else if Re.execp reg_rcurly input then
    Tok_RCurly :: (tokenize (reduce_input input 1))
else if Re.execp reg_dot input then
    Tok_Dot :: (tokenize (reduce_input input 1))
else if Re.execp reg_equal input then
    Tok_Equal :: (tokenize (reduce_input input 1))
else if Re.execp reg_notequal input then
    Tok_NotEqual :: (tokenize (reduce_input input 2))
else if Re.execp reg_geq input then
    Tok_GreaterEqual :: (tokenize (reduce_input input 2))
else if Re.execp reg_leq input then
    Tok_LessEqual :: (tokenize (reduce_input input 2))
else if Re.execp reg_less input then
    Tok_Less :: (tokenize (reduce_input input 1))
else if Re.execp reg_greater input then
    Tok_Greater :: (tokenize (reduce_input input 1))
else if Re.execp reg_or input then
    Tok_Or :: (tokenize (reduce_input input 2))
else if Re.execp reg_and input then
    Tok_And :: (tokenize (reduce_input input 2))
else if Re.execp reg_add input then
    Tok_Add :: (tokenize (reduce_input input 1))
else if Re.execp reg_arrow input then
    Tok_Arrow :: (tokenize (reduce_input input 2))
else if Re.execp reg_sub input then
    Tok_Sub :: (tokenize (reduce_input input 1)) 
else if Re.execp reg_mult input then
    Tok_Mult :: (tokenize (reduce_input input 1))
else if Re.execp reg_div input then
    Tok_Div :: (tokenize (reduce_input input 1))
else if Re.execp reg_concat input then
    Tok_Concat :: (tokenize (reduce_input input 1))
else if Re.execp reg_dubsemi input then
    Tok_DoubleSemi :: (tokenize (reduce_input input 2))
else if Re.execp reg_semi input then
    Tok_Semi :: (tokenize (reduce_input input 1))
else if Re.execp reg_id_5 input then
    let (ans,len) = grab_token reg_id_5 input in
    Tok_ID(ans)::(tokenize (String.sub input len ((String.length input) - len)))
else if Re.execp reg_then input then 
    Tok_Then :: (tokenize (reduce_input input 4))
else if Re.execp reg_else input then 
    Tok_Else :: (tokenize (reduce_input input 4))
else if Re.execp reg_id_4 input then
    let (ans,len) = grab_token reg_id_4 input in
    Tok_ID(ans)::(tokenize (String.sub input len ((String.length input) - len)))
else if Re.execp reg_rec input then 
    Tok_Rec :: (tokenize (reduce_input input 3))
else if Re.execp reg_fun input then 
    Tok_Fun :: (tokenize (reduce_input input 3))
else if Re.execp reg_not input then 
    Tok_Not :: (tokenize (reduce_input input 3))
else if Re.execp reg_let input then (**)
    Tok_Let :: (tokenize (reduce_input input 3))
else if Re.execp reg_def input then 
    Tok_Def :: (tokenize (reduce_input input 3))
else if Re.execp reg_id_3 input then
    let (ans,len) = grab_token reg_id_3 input in
    Tok_ID(ans)::(tokenize (String.sub input len ((String.length input) - len)))
else if Re.execp reg_in input then
    Tok_In :: (tokenize (reduce_input input 2))
else if Re.execp reg_if input then 
    Tok_If :: (tokenize (reduce_input input 2))
else if Re.execp reg_id input then
    let (ans,len) = grab_token reg_id input in
    Tok_ID(ans)::(tokenize (String.sub input len ((String.length input) - len)))
else raise (InvalidInputException "not a valid word")
