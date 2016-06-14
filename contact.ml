type field = All | Id | Name | Surname | Age | Email | Phone;;
type contact = {mutable surname : string; mutable name :string;mutable age : int; mutable email : string; mutable phone : string};;

exception Add_Contact_With_Invalid_Data
exception Remove_Using_An_Invalid_Id
exception Remove_Impossible_On_An_Empty_List
exception Replace_Impossible_On_An_Empty_List
exception Replace_Using_An_Invalid_Id
exception Replace_Contact_With_Invalid_Data

let check_phone phone mode =
     if String.length(phone) != 14 || phone.[0] != '0' || phone.[1] < '1' || phone.[1] > '9' || phone.[2] != ' ' || phone.[3] < '0' || phone.[3] > '9' || phone.[4] < '0' || phone.[4] > '9' || phone.[5] != ' ' || phone.[6] < '0' || phone.[6] > '9' || phone.[7] < '0' || phone.[7] > '9' || phone.[8] != ' ' || phone.[9] < '0' || phone.[9] > '9' || phone.[10] < '0' || phone.[10] > '9' || phone.[11] != ' ' || phone.[12] < '0' || phone.[12] > '9' || phone.[13] < '0' || phone.[13] > '9' then
          if mode then raise Add_Contact_With_Invalid_Data else raise Replace_Contact_With_Invalid_Data
     else phone

let check_email email mode =
     try
          let atpos = String.index email '@' in
          let dotpos = String.index_from email atpos '.' in
          if dotpos = (atpos + 1) || atpos = 0 || dotpos = ((String.length email) - 1) then raise Not_found else email
     with Not_found -> if mode then raise Add_Contact_With_Invalid_Data else raise Replace_Contact_With_Invalid_Data

let rec capitalize_rec name index =
     if index >= String.length name then name
     else
     (if name.[index] >= 'a' && name.[index] <= 'z' && (index = 0 || name.[index - 1] = '\'' || name.[index - 1] = ' ' || name.[index - 1] == '-') then
          String.set name index (Char.uppercase name.[index])
     else if name.[index] >= 'A' && name.[index] <= 'Z' && index != 0 && name.[index - 1] != '\'' && name.[index - 1] != ' ' && name.[index - 1] != '-' then
          String.set name index (Char.lowercase name.[index])
     ;capitalize_rec name (index + 1))

let capitalize name = capitalize_rec name 0

let add list (surname, name, age, email, phone) = match surname with
     | "" -> raise Add_Contact_With_Invalid_Data
     | _ -> match name with
          | "" -> raise Add_Contact_With_Invalid_Data
          | _ -> match age with
               | f when f < 0 -> raise Add_Contact_With_Invalid_Data
               | f when f > 120 -> raise Add_Contact_With_Invalid_Data
               | _ -> list@[{surname = capitalize(String.trim(surname)); name = String.trim(String.uppercase(name)); age = age; email = check_email email true; phone = check_phone phone true}]

let rec getId_rec list field_type argument index result = match index with
     | index_ when index_ >= List.length(list) -> result
     | _ -> match field_type with
          | All when argument = string_of_int(index)
               || argument = String.lowercase((List.nth list index).name)
               || argument = String.lowercase((List.nth list index).surname)
               || argument = string_of_int((List.nth list index).age)
               || argument = String.lowercase((List.nth list index).email)
               || argument = (List.nth list index).phone -> getId_rec list field_type argument (index + 1) index
          | Id when argument = string_of_int(index) -> getId_rec list field_type argument (index + 1) index
          | Name when argument = String.lowercase((List.nth list index).name) -> getId_rec list field_type argument (index + 1) index
          | Surname when argument = String.lowercase((List.nth list index).surname) -> getId_rec list field_type argument (index + 1) index
          | Age when argument = string_of_int((List.nth list index).age) -> getId_rec list field_type argument (index + 1) index
          | Email when argument = String.lowercase((List.nth list index).email) -> getId_rec list field_type argument (index + 1) index
          | Phone when argument = (List.nth list index).phone -> getId_rec list field_type argument (index + 1) index
          | _ -> getId_rec list field_type argument (index + 1) result

let getId list field_type argument = getId_rec list field_type (String.lowercase argument) 0 (-1)

let remove list index =
     if List.length list = 0 then
          raise Remove_Impossible_On_An_Empty_List
     else if index < 0 || index >= List.length list then
          raise Remove_Using_An_Invalid_Id
     else
          let rec remove_rec current _list _list2 = match _list2 with
               | [] -> List.rev _list
               | head :: tail -> if index != current then remove_rec (current + 1) (head :: _list) tail else remove_rec (current + 1) _list tail
          in remove_rec 0 [] list

let replace list index (surname, name, age, email, phone) =
     if List.length list = 0 then
          raise Replace_Impossible_On_An_Empty_List
     else if index < 0 || index >= List.length list then
          raise Replace_Using_An_Invalid_Id
     else
          let rec replace_rec current _list _list2 = match _list2 with
               | [] -> List.rev _list
               | head :: tail -> replace_rec (current + 1) ( if index != current then (head :: _list) else {surname = capitalize(String.trim(surname)); name = String.trim(String.uppercase(name)); age = age; email = check_email email false; phone = check_phone phone false} :: _list ) tail
          in replace_rec 0 [] list

let rec print_spaces count =
     if count <= 0 then () else begin print_string " " ; print_spaces (count - 1) end

let my_print_string message length =
     if String.length message > length then print_string (String.sub message 0 length) else begin print_string message ; print_spaces (length - (String.length message)) end

let rec print_rec list field_type argument index = match index with
     | index_ when index_ >= List.length(list) -> ()
     | _ ->
     let print_contact list field_type argument contact index =
        my_print_string (string_of_int(index)) 4 ;
        my_print_string contact.surname 16 ;
        my_print_string contact.name 16 ;
        my_print_string (string_of_int(contact.age)) 4 ;
        my_print_string contact.email 32 ;
        my_print_string contact.phone 14 ;
        print_string "\n"
        ; print_rec list field_type argument (index + 1) in match field_type with
          | Id when argument = string_of_int(index) -> print_contact list field_type argument (List.nth list index) index
          | Name when argument = String.lowercase((List.nth list index).name) -> print_contact list field_type argument (List.nth list index) index
          | Surname when argument = String.lowercase((List.nth list index).surname) -> print_contact list field_type argument (List.nth list index) index
          | Age when argument = string_of_int((List.nth list index).age) -> print_contact list field_type argument (List.nth list index) index
          | Email when argument = String.lowercase((List.nth list index).email) -> print_contact list field_type argument (List.nth list index) index
          | Phone when argument = (List.nth list index).phone -> print_contact list field_type argument (List.nth list index) index
          | _ -> print_rec list field_type argument (index + 1)

let print list field_type argument = print_rec list field_type (String.lowercase argument) 0

(*        Printf.printf "%-4d %-16s %-16s %-4d %-32s %-14s\n" index contact.surname contact.name contact.age contact.email contact.phone *)
