(* Copyright Stephen Weeks (sweeks@sweeks.com).  1999-6-21.
 *
 * This code solves the following "zebra" puzzle, and prints the solution.
 * There are 120^5 ~= 24 billion possibilities, so exhaustive search should
 * work fine, but I decided to write something that was a bit more clever.
 * It took me longer to write (2.5 hours) than to write exhaustive search, but
 * it runs fast (0.06 seconds on my 400MhZ P6). The code only needs to explore
 * 3342 posibilites to solve the puzzle.
 *
 * Here is the puzzle.
 *
 * This word problem has 25 variables and 24 are given values. You must
 * solve 
 * the 25th. 
 * 
 * The trick is HOW? 
 * 
 * If you look at the problem mathematically, no sweat. If you get lost
 * in the 
 * English, you are dead. 
 * 
 * You will know you are right by checking the answer with all the
 * conditions. 
 * 
 * Less than 1 percent of the population can solve this problem. 
 * 
 * The question is: Based on the following clues, who owns the zebra? 
 * 
 * **There are five houses. 
 * 
 * **Each house has its own unique color. 
 * 
 * **All house owners are of different nationalities. 
 * 
 * **They all have different pets. 
 * 
 * **They all drink different drinks. 
 * 
 * **They all smoke different cigarettes. 
 * 
 * **The Englishman lives in the red house. 
 * 
 * **The Swede has a dog. 
 * 
 * **The Dane drinks tea. 
 * 
 * **The green house is adjacent to the white house on the left. 
 * 
 * **In the green house they drink coffee. 
 * 
 * **The man who smokes Pall Malls has birds. 
 * 
 * **In the yellow house they smoke Dunhills. 
 * 
 * **In the middle house they drink milk. 
 * 
 * **The Norwegian lives in the first house. 
 * 
 * **The man who smokes Blends lives in a house next to the house with
 * cats. 
 * 
 * **In a house next to the house where they have a horse, they smoke 
 * Dunhills. 
 * 
 * **The man who smokes Blue Masters drinks beer. 
 * 
 * **The German smokes Princes. 
 * 
 * **The Norwegian lives next to the blue house. 
 * 
 * **They drink water in a house next to the house where they smoke
 * Blends. 
 * 
 * Who owns the zebra? 
 *)

fun peek (l, p) = List.find p l
fun map (l, f) = List.map f l
fun fold (l, b, f) = List.foldl f b l

datatype cigarette = Blend | BlueMaster | Dunhill | PallMall | Prince
val cigaretteToString =
   fn Blend => "Blend"
    | BlueMaster => "BlueMaster"
    | Dunhill => "Dunhill"
    | PallMall => "PallMall"
    | Prince => "Prince"
datatype color = Blue | Green | Red | White | Yellow
val colorToString =
   fn Blue => "Blue"
    | Green => "Green"
    | Red => "Red"
    | White => "White"
    | Yellow => "Yellow"
datatype drink = Beer | Coffee | Milk | Tea | Water
val drinkToString =
   fn Beer => "Beer"
    | Coffee => "Coffee"
    | Milk => "Milk"
    | Tea => "Tea"
    | Water => "Water"
datatype nationality = Dane | English | German | Norwegian | Swede
val nationalityToString =
   fn Dane => "Dane"
    | English => "English"
    | German => "German"
    | Norwegian => "Norwegian"
    | Swede => "Swede"
datatype pet = Bird | Cat | Dog | Horse | Zebra
val petToString =
   fn Bird => "Bird"
    | Cat => "Cat"
    | Dog => "Dog"
    | Horse => "Horse"
    | Zebra => "Zebra"

type pos = int
val poss = [1, 2, 3, 4, 5]
val first = SOME 1
val middle = SOME 3

type 'a attribute = {poss: pos list,
                     unknown: 'a list,
                     known: (pos * 'a) list}

exception Done
fun 'a fluidLet (r: 'a ref, x: 'a, f: unit -> 'b): 'b =
   let val old = !r
   in r := x
      ; (f () before r := old)
      handle Done => raise Done
           | e => (r := old; raise e)
   end

fun search () =
   let
      fun init (unknown: 'a list): 'a attribute ref =
         ref {poss = poss, unknown = unknown, known = []}
      val cigarettes = init [Blend, BlueMaster, Dunhill, PallMall, Prince]
      val colors = init [Blue, Green, Red, White, Yellow]
      val drinks = init [Beer, Coffee, Milk, Tea, Water]
      val nationalities = init [Dane, English, German, Norwegian, Swede]
      val pets = init [Bird, Cat, Dog, Horse, Zebra]

      fun ''a find (r: ''a attribute ref) (x: ''a): pos option =
         Option.map #1 (peek (#known (!r), fn (_, y) => x = y))
      val smoke = find cigarettes
      val color = find colors
      val drink = find drinks
      val nat = find nationalities
      val pet = find pets

      fun display () =
         let
            fun loop (r: 'a attribute ref, toString) =
               (List.app (fn i =>
                          let
                             val x = #2 (valOf (peek (#known (!r),
                                                   fn (j, _) => i = j)))
                             val s = toString x
                          in print s
                             ; print (CharVector.tabulate (12 - size s,
                                                         fn _ => #" "))
                          end) poss 
                ; print "\n")
         in
            loop (cigarettes, cigaretteToString)
            ; loop (colors, colorToString)
            ; loop (drinks, drinkToString)
            ; loop (nationalities, nationalityToString)
            ; loop (pets, petToString)
         end
   
      fun make f =
         fn (SOME x, SOME y) => f (x, y)
          | _ => true
      val same = make (op =)
      val adjacent = make (fn (x, y) => x = y - 1 orelse y = x - 1)
      val left = make (fn (x, y) => x = y - 1)

      val num = ref 0
      fun isConsistent (): bool =
         (num := !num + 1
          ;
         same (nat English, color Red)
         andalso same (nat Swede, pet Dog)
         andalso same (nat Dane, drink Tea)
         andalso left (color Green, color White)
         andalso same (color Green, drink Coffee)
         andalso same (smoke PallMall, pet Bird)
         andalso same (color Yellow, smoke Dunhill)
         andalso same (middle, drink Milk)
         andalso same (nat Norwegian, first)
         andalso adjacent (smoke Blend, pet Cat)
         andalso adjacent (pet Horse, smoke Dunhill)
         andalso same (drink Beer, smoke BlueMaster)
         andalso same (nat German, smoke Prince)
         andalso adjacent (nat Norwegian, color Blue)
         andalso adjacent (drink Water, smoke Blend)
          )
         
      fun tryEach (l, f) =
         let
            fun loop (l, ac) =
               case l of
                  [] => ()
                | x :: l => (f (x, l @ ac); loop (l, x :: ac))
         in loop (l, [])
         end
               
      fun try (r: 'a attribute ref,
              f: unit -> (('a attribute -> unit)
                          * ( unit -> unit))) =
         let val {poss, unknown, known} = !r
         in case unknown of
            [] => ()
          | _ => 
               tryEach (unknown, fn (x, unknown) =>
                       let val (each, done) = f ()
                       in tryEach (poss, fn (p, poss) =>
                                  let val attr = {known = (p, x) :: known,
                                                  unknown = unknown,
                                                  poss = poss}
                                  in fluidLet
                                     (r, attr, fn () =>
                                      if isConsistent () then each attr else ())
                                  end)
                          ; done ()
                       end)
         end

      (* loop takes the current state and either
       *   - terminates in the same state if there is no consistent extension
       *   - raises Done with the state set at the consistent extension
       *)
      exception Inconsistent
      exception Continue of unit -> unit
      fun loop (): unit =
         let
            fun test r =
               try
               (r, fn () =>
                let
                   datatype 'a attrs = None | One of 'a | Many
                   val attrs = ref None
                   fun each a =
                      case !attrs of
                         None => attrs := One a
                       | One _ => attrs := Many
                       | Many => ()
                   fun done () =
                      case !attrs of
                         None => raise Inconsistent
                       | One a => raise (Continue (fn () => fluidLet (r, a, loop)))
                       | Many => ()
                in (each, done)
                end)
            fun explore r =
               try (r, fn () =>
                   let
                      fun each _ = loop ()
                      fun done () = raise Inconsistent
                   in (each, done)
                   end)
         in (test cigarettes
             ; test colors
             ; test drinks
             ; test nationalities
             ; test pets
             ; explore cigarettes
             ; explore colors
             ; explore drinks
             ; explore nationalities
             ; explore pets
             ; raise Done)
            handle Inconsistent => ()
                 | Continue f => f ()
         end
      val _ =    loop () handle Done => ()
      val _ = if 3342 = !num
                 then ()
              else raise Fail "bug"
(*      val _ = display () *)
   in ()
   end

structure Main =
   struct
      fun doit n =
         let
            fun loop n =
               if n < 0
                  then ()
               else (search ()
                     ; loop (n - 1))
         in loop (n * 1000)
         end
   end
