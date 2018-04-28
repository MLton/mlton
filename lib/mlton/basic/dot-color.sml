(* Copyright (C) 1999-2006 Henry Cejtin, Matthew Fluet, Suresh
 *    Jagannathan, and Stephen Weeks.
 *
 * MLton is released under a HPND-style license.
 * See the file MLton-LICENSE for details.
 *)

structure DotColor =
   struct
      datatype t =
         (* (Hue, Saturation, Brightness).  All between 0 and 1. *)
         HSB of real * real * real
       | Aliceblue
       | Antiquewhite1 | Antiquewhite2 | Antiquewhite3 | Antiquewhite4
       | Aquamarine1 | Aquamarine2 | Aquamarine3 | Aquamarine4
       | Azure1 | Azure2 | Azure3 | Azure4
       | Beige
       | Bisque1 | Bisque2 | Bisque3 | Bisque4
       | Black
       | Blanchedalmond
       | Blue1 | Blue2 | Blue3 | Blue4
       | Blueviolet
       | Brown1 | Brown2 | Brown3 | Brown4
       | Burlywood1 | Burlywood2 | Burlywood3 | Burlywood4
       | Cadetblue1 | Cadetblue2 | Cadetblue3 | Cadetblue4
       | Chartreuse1 | Chartreuse2 | Chartreuse3 | Chartreuse4
       | Chocolate1 | Chocolate2 | Chocolate3 | Chocolate4
       | Coral1 | Coral2 | Coral3 | Coral4
       | Corn
       | Cornsilk1 | Cornsilk2 | Cornsilk3 | Cornsilk4
       | Crimson
       | Cyan1 | Cyan2 | Cyan3 | Cyan4
       | Darkgoldenrod1 | Darkgoldenrod2 | Darkgoldenrod3 | Darkgoldenrod4
       | Darkgreen
       | Darkkhaki
       | Darkolivegreen1 | Darkolivegreen2 | Darkolivegreen3 | Darkolivegreen4
       | Darkorange1 | Darkorange2 | Darkorange3 | Darkorange4
       | Darkorchid1 | Darkorchid2 | Darkorchid3 | Darkorchid4
       | Darksalmon
       | Darkseagreen1 | Darkseagreen2 | Darkseagreen3 | Darkseagreen4
       | Darkslateblue
       | Darkslategray1 | Darkslategray2 | Darkslategray3 | Darkslategray4
       | Darkturquoise
       | Darkviolet
       | Deeppink1 | Deeppink2 | Deeppink3 | Deeppink4
       | Deepskyblue1 | Deepskyblue2 | Deepskyblue3 | Deepskyblue4
       | Dimgray
       | Dodgerblue1 | Dodgerblue2 | Dodgerblue3 | Dodgerblue4
       | Forestgreen
       | Gainsboro
       | Ghostwhite
       | Gold1 | Gold2 | Gold3 | Gold4
       | Goldenrod1 | Goldenrod2 | Goldenrod3 | Goldenrod4
       | Gray
       | Gray0 | Gray1 | Gray2 | Gray3 | Gray4 | Gray5 | Gray6 | Gray7 | Gray8
       | Gray9 | Gray10 | Gray11 | Gray12 | Gray13 | Gray14 | Gray15 | Gray16
       | Gray17 | Gray18 | Gray19 | Gray20 | Gray21 | Gray22 | Gray23 | Gray24
       | Gray25 | Gray26 | Gray27 | Gray28 | Gray29 | Gray30 | Gray31 | Gray32
       | Gray33 | Gray34 | Gray35 | Gray36 | Gray37 | Gray38 | Gray39 | Gray40
       | Gray41 | Gray42 | Gray43 | Gray44 | Gray45 | Gray46 | Gray47 | Gray48
       | Gray49 | Gray50 | Gray51 | Gray52 | Gray53 | Gray54 | Gray55 | Gray56
       | Gray57 | Gray58 | Gray59 | Gray60 | Gray61 | Gray62 | Gray63 | Gray64
       | Gray65 | Gray66 | Gray67 | Gray68 | Gray69 | Gray70 | Gray71 | Gray72
       | Gray73 | Gray74 | Gray75 | Gray76 | Gray77 | Gray78 | Gray79 | Gray80
       | Gray81 | Gray82 | Gray83 | Gray84 | Gray85 | Gray86 | Gray87 | Gray88
       | Gray89 | Gray90 | Gray91 | Gray92 | Gray93 | Gray94 | Gray95 | Gray96
       | Gray97 | Gray98 | Gray99 | Gray100
       | Green1 | Green2 | Green3 | Green4
       | Greenyellow
       | Honeydew1 | Honeydew2 | Honeydew3 | Honeydew4
       | Hotpink1 | Hotpink2 | Hotpink3 | Hotpink4
       | Indianred1 | Indianred2 | Indianred3 | Indianred4
       | Indigo
       | Ivory1 | Ivory2 | Ivory3 | Ivory4
       | Khaki1 | Khaki2 | Khaki3 | Khaki4
       | Lavender
       | Lavenderblush1 | Lavenderblush2 | Lavenderblush3 | Lavenderblush4
       | Lawngreen
       | Lemonchi
       | Lightblue1 | Lightblue2 | Lightblue3 | Lightblue4
       | Lightcyan1 | Lightcyan2 | Lightcyan3 | Lightcyan4
       | Lightgoldenrod1 | Lightgoldenrod2 | Lightgoldenrod3 | Lightgoldenrod4
       | Lightgoldenrodyellow
       | Lightgray
       | Lightpink1 | Lightpink2 | Lightpink3 | Lightpink4
       | Lightsalmon1 | Lightsalmon2 | Lightsalmon3 | Lightsalmon4
       | Lightseagreen
       | Lightskyblue1 | Lightskyblue2 | Lightskyblue3 | Lightskyblue4
       | Lightslateblue1 | Lightslateblue2 | Lightslateblue3 | Lightslateblue4
       | Lightslategray
       | Lightyellow1 | Lightyellow2 | Lightyellow3 | Lightyellow4
       | Limegreen
       | Linen
       | Magenta1 | Magenta2 | Magenta3 | Magenta4
       | Maroon1 | Maroon2 | Maroon3 | Maroon4
       | Mediumaquamarine
       | Mediumblue
       | Mediumorchid1 | Mediumorchid2 | Mediumorchid3 | Mediumorchid4
       | Mediumpurple1 | Mediumpurple2 | Mediumpurple3 | Mediumpurple4
       | Mediumseagreen
       | Mediumslateblue
       | Mediumspringgreen
       | Mediumturquoise
       | Mediumvioletred
       | Midnightblue
       | Mintcream
       | Mistyrose1 | Mistyrose2 | Mistyrose3 | Mistyrose4
       | Moccasin
       | Navajowhite1 | Navajowhite2 | Navajowhite3 | Navajowhite4
       | Navy
       | Navyblue
       | Oldlace
       | Olivedrab1 | Olivedrab2 | Olivedrab3 | Olivedrab4
       | On1 | On2 | On3 | On4
       | Oralwhite
       | Orange1 | Orange2 | Orange3 | Orange4
       | Orangered1 | Orangered2 | Orangered3 | Orangered4
       | Orchid1 | Orchid2 | Orchid3 | Orchid4
       | Owerblue
       | Palegoldenrod
       | Palegreen1 | Palegreen2 | Palegreen3 | Palegreen4
       | Paleturquoise1 | Paleturquoise2 | Paleturquoise3 | Paleturquoise4
       | Palevioletred1 | Palevioletred2 | Palevioletred3 | Palevioletred4
       | Papayawhip
       | Peachpu1 | Peachpu2 | Peachpu3 | Peachpu4
       | Peru
       | Pink1 | Pink2 | Pink3 | Pink4
       | Plum1 | Plum2 | Plum3 | Plum4
       | Powderblue
       | Purple1 | Purple2 | Purple3 | Purple4
       | Rebrick1 | Rebrick2 | Rebrick3 | Rebrick4
       | Red1 | Red2 | Red3 | Red4
       | Rosybrown1 | Rosybrown2 | Rosybrown3 | Rosybrown4
       | Royalblue1 | Royalblue2 | Royalblue3 | Royalblue4
       | Saddlebrown
       | Salmon1 | Salmon2 | Salmon3 | Salmon4
       | Sandybrown
       | Seagreen1 | Seagreen2 | Seagreen3 | Seagreen4
       | Seashell1 | Seashell2 | Seashell3 | Seashell4
       | Sienna1 | Sienna2 | Sienna3 | Sienna4
       | Skyblue1 | Skyblue2 | Skyblue3 | Skyblue4
       | Slateblue1 | Slateblue2 | Slateblue3 | Slateblue4
       | Slategray1 | Slategray2 | Slategray3 | Slategray4
       | Snow1 | Snow2 | Snow3 | Snow4
       | Springgreen1 | Springgreen2 | Springgreen3 | Springgreen4
       | Steelblue1 | Steelblue2 | Steelblue3 | Steelblue4
       | Tan1 | Tan2 | Tan3 | Tan4
       | Thistle1 | Thistle2 | Thistle3 | Thistle4
       | Tomato1 | Tomato2 | Tomato3 | Tomato4
       | Turquoise1 | Turquoise2 | Turquoise3 | Turquoise4
       | Violet
       | Violetred1 | Violetred2 | Violetred3 | Violetred4
       | Wheat1 | Wheat2 | Wheat3 | Wheat4
       | White
       | Whitesmoke
       | Yellow1 | Yellow2 | Yellow3 | Yellow4
       | Yellowgreen


      val grays =
         Vector.fromList
         [Gray0, Gray1, Gray2, Gray3, Gray4, Gray5, Gray6, Gray7, Gray8, Gray9, Gray10, Gray11, Gray12, Gray13, Gray14, Gray15, Gray16, Gray17, Gray18, Gray19, Gray20, Gray21, Gray22, Gray23, Gray24, Gray25, Gray26, Gray27, Gray28, Gray29, Gray30, Gray31, Gray32, Gray33, Gray34, Gray35, Gray36, Gray37, Gray38, Gray39, Gray40, Gray41, Gray42, Gray43, Gray44, Gray45, Gray46, Gray47, Gray48, Gray49, Gray50, Gray51, Gray52, Gray53, Gray54, Gray55, Gray56, Gray57, Gray58, Gray59, Gray60, Gray61, Gray62, Gray63, Gray64, Gray65, Gray66, Gray67, Gray68, Gray69, Gray70, Gray71, Gray72, Gray73, Gray74, Gray75, Gray76, Gray77, Gray78, Gray79, Gray80, Gray81, Gray82, Gray83, Gray84, Gray85, Gray86, Gray87, Gray88, Gray89, Gray90, Gray91, Gray92, Gray93, Gray94, Gray95, Gray96, Gray97, Gray98, Gray99, Gray100]

      fun gray i =
         if 0 <= i andalso i < Vector.length grays
            then Vector.sub (grays, i)
         else Error.bug "Dot.gray"

      fun realToString x = Real.format (x, Real.Format.fix (SOME 2))

      val toString =
         fn HSB (h, s, b) => concat [realToString h, " ",
                                     realToString s, " ",
                                     realToString b]
          | Aliceblue => "Aliceblue"
          | Antiquewhite1 => "Antiquewhite1"
          | Antiquewhite2 => "Antiquewhite2"
          | Antiquewhite3 => "Antiquewhite3"
          | Antiquewhite4 => "Antiquewhite4"
          | Aquamarine1 => "Aquamarine1"
          | Aquamarine2 => "Aquamarine2"
          | Aquamarine3 => "Aquamarine3"
          | Aquamarine4 => "Aquamarine4"
          | Azure1 => "Azure1"
          | Azure2 => "Azure2"
          | Azure3 => "Azure3"
          | Azure4 => "Azure4"
          | Beige => "Beige"
          | Bisque1 => "Bisque1"
          | Bisque2 => "Bisque2"
          | Bisque3 => "Bisque3"
          | Bisque4 => "Bisque4"
          | Black => "Black"
          | Blanchedalmond => "Blanchedalmond"
          | Blue1 => "Blue1"
          | Blue2 => "Blue2"
          | Blue3 => "Blue3"
          | Blue4 => "Blue4"
          | Blueviolet => "Blueviolet"
          | Brown1 => "Brown1"
          | Brown2 => "Brown2"
          | Brown3 => "Brown3"
          | Brown4 => "Brown4"
          | Burlywood1 => "Burlywood1"
          | Burlywood2 => "Burlywood2"
          | Burlywood3 => "Burlywood3"
          | Burlywood4 => "Burlywood4"
          | Cadetblue1 => "Cadetblue1"
          | Cadetblue2 => "Cadetblue2"
          | Cadetblue3 => "Cadetblue3"
          | Cadetblue4 => "Cadetblue4"
          | Chartreuse1 => "Chartreuse1"
          | Chartreuse2 => "Chartreuse2"
          | Chartreuse3 => "Chartreuse3"
          | Chartreuse4 => "Chartreuse4"
          | Chocolate1 => "Chocolate1"
          | Chocolate2 => "Chocolate2"
          | Chocolate3 => "Chocolate3"
          | Chocolate4 => "Chocolate4"
          | Coral1 => "Coral1"
          | Coral2 => "Coral2"
          | Coral3 => "Coral3"
          | Coral4 => "Coral4"
          | Corn => "Corn"
          | Cornsilk1 => "Cornsilk1"
          | Cornsilk2 => "Cornsilk2"
          | Cornsilk3 => "Cornsilk3"
          | Cornsilk4 => "Cornsilk4"
          | Crimson => "Crimson"
          | Cyan1 => "Cyan1"
          | Cyan2 => "Cyan2"
          | Cyan3 => "Cyan3"
          | Cyan4 => "Cyan4"
          | Darkgoldenrod1 => "Darkgoldenrod1"
          | Darkgoldenrod2 => "Darkgoldenrod2"
          | Darkgoldenrod3 => "Darkgoldenrod3"
          | Darkgoldenrod4 => "Darkgoldenrod4"
          | Darkgreen => "Darkgreen"
          | Darkkhaki => "Darkkhaki"
          | Darkolivegreen1 => "Darkolivegreen1"
          | Darkolivegreen2 => "Darkolivegreen2"
          | Darkolivegreen3 => "Darkolivegreen3"
          | Darkolivegreen4 => "Darkolivegreen4"
          | Darkorange1 => "Darkorange1"
          | Darkorange2 => "Darkorange2"
          | Darkorange3 => "Darkorange3"
          | Darkorange4 => "Darkorange4"
          | Darkorchid1 => "Darkorchid1"
          | Darkorchid2 => "Darkorchid2"
          | Darkorchid3 => "Darkorchid3"
          | Darkorchid4 => "Darkorchid4"
          | Darksalmon => "Darksalmon"
          | Darkseagreen1 => "Darkseagreen1"
          | Darkseagreen2 => "Darkseagreen2"
          | Darkseagreen3 => "Darkseagreen3"
          | Darkseagreen4 => "Darkseagreen4"
          | Darkslateblue => "Darkslateblue"
          | Darkslategray1 => "Darkslategray1"
          | Darkslategray2 => "Darkslategray2"
          | Darkslategray3 => "Darkslategray3"
          | Darkslategray4 => "Darkslategray4"
          | Darkturquoise => "Darkturquoise"
          | Darkviolet => "Darkviolet"
          | Deeppink1 => "Deeppink1"
          | Deeppink2 => "Deeppink2"
          | Deeppink3 => "Deeppink3"
          | Deeppink4 => "Deeppink4"
          | Deepskyblue1 => "Deepskyblue1"
          | Deepskyblue2 => "Deepskyblue2"
          | Deepskyblue3 => "Deepskyblue3"
          | Deepskyblue4 => "Deepskyblue4"
          | Dimgray => "Dimgray"
          | Dodgerblue1 => "Dodgerblue1"
          | Dodgerblue2 => "Dodgerblue2"
          | Dodgerblue3 => "Dodgerblue3"
          | Dodgerblue4 => "Dodgerblue4"
          | Forestgreen => "Forestgreen"
          | Gainsboro => "Gainsboro"
          | Ghostwhite => "Ghostwhite"
          | Gold1 => "Gold1"
          | Gold2 => "Gold2"
          | Gold3 => "Gold3"
          | Gold4 => "Gold4"
          | Goldenrod1 => "Goldenrod1"
          | Goldenrod2 => "Goldenrod2"
          | Goldenrod3 => "Goldenrod3"
          | Goldenrod4 => "Goldenrod4"
          | Gray => "Gray"
          | Gray0 => "Gray0"
          | Gray1 => "Gray1"
          | Gray2 => "Gray2"
          | Gray3 => "Gray3"
          | Gray4 => "Gray4"
          | Gray5 => "Gray5"
          | Gray6 => "Gray6"
          | Gray7 => "Gray7"
          | Gray8 => "Gray8"
          | Gray9 => "Gray9"
          | Gray10 => "Gray10"
          | Gray11 => "Gray11"
          | Gray12 => "Gray12"
          | Gray13 => "Gray13"
          | Gray14 => "Gray14"
          | Gray15 => "Gray15"
          | Gray16 => "Gray16"
          | Gray17 => "Gray17"
          | Gray18 => "Gray18"
          | Gray19 => "Gray19"
          | Gray20 => "Gray20"
          | Gray21 => "Gray21"
          | Gray22 => "Gray22"
          | Gray23 => "Gray23"
          | Gray24 => "Gray24"
          | Gray25 => "Gray25"
          | Gray26 => "Gray26"
          | Gray27 => "Gray27"
          | Gray28 => "Gray28"
          | Gray29 => "Gray29"
          | Gray30 => "Gray30"
          | Gray31 => "Gray31"
          | Gray32 => "Gray32"
          | Gray33 => "Gray33"
          | Gray34 => "Gray34"
          | Gray35 => "Gray35"
          | Gray36 => "Gray36"
          | Gray37 => "Gray37"
          | Gray38 => "Gray38"
          | Gray39 => "Gray39"
          | Gray40 => "Gray40"
          | Gray41 => "Gray41"
          | Gray42 => "Gray42"
          | Gray43 => "Gray43"
          | Gray44 => "Gray44"
          | Gray45 => "Gray45"
          | Gray46 => "Gray46"
          | Gray47 => "Gray47"
          | Gray48 => "Gray48"
          | Gray49 => "Gray49"
          | Gray50 => "Gray50"
          | Gray51 => "Gray51"
          | Gray52 => "Gray52"
          | Gray53 => "Gray53"
          | Gray54 => "Gray54"
          | Gray55 => "Gray55"
          | Gray56 => "Gray56"
          | Gray57 => "Gray57"
          | Gray58 => "Gray58"
          | Gray59 => "Gray59"
          | Gray60 => "Gray60"
          | Gray61 => "Gray61"
          | Gray62 => "Gray62"
          | Gray63 => "Gray63"
          | Gray64 => "Gray64"
          | Gray65 => "Gray65"
          | Gray66 => "Gray66"
          | Gray67 => "Gray67"
          | Gray68 => "Gray68"
          | Gray69 => "Gray69"
          | Gray70 => "Gray70"
          | Gray71 => "Gray71"
          | Gray72 => "Gray72"
          | Gray73 => "Gray73"
          | Gray74 => "Gray74"
          | Gray75 => "Gray75"
          | Gray76 => "Gray76"
          | Gray77 => "Gray77"
          | Gray78 => "Gray78"
          | Gray79 => "Gray79"
          | Gray80 => "Gray80"
          | Gray81 => "Gray81"
          | Gray82 => "Gray82"
          | Gray83 => "Gray83"
          | Gray84 => "Gray84"
          | Gray85 => "Gray85"
          | Gray86 => "Gray86"
          | Gray87 => "Gray87"
          | Gray88 => "Gray88"
          | Gray89 => "Gray89"
          | Gray90 => "Gray90"
          | Gray91 => "Gray91"
          | Gray92 => "Gray92"
          | Gray93 => "Gray93"
          | Gray94 => "Gray94"
          | Gray95 => "Gray95"
          | Gray96 => "Gray96"
          | Gray97 => "Gray97"
          | Gray98 => "Gray98"
          | Gray99 => "Gray99"
          | Gray100 => "Gray100"
          | Green1 => "Green1"
          | Green2 => "Green2"
          | Green3 => "Green3"
          | Green4 => "Green4"
          | Greenyellow => "Greenyellow"
          | Honeydew1 => "Honeydew1"
          | Honeydew2 => "Honeydew2"
          | Honeydew3 => "Honeydew3"
          | Honeydew4 => "Honeydew4"
          | Hotpink1 => "Hotpink1"
          | Hotpink2 => "Hotpink2"
          | Hotpink3 => "Hotpink3"
          | Hotpink4 => "Hotpink4"
          | Indianred1 => "Indianred1"
          | Indianred2 => "Indianred2"
          | Indianred3 => "Indianred3"
          | Indianred4 => "Indianred4"
          | Indigo => "Indigo"
          | Ivory1 => "Ivory1"
          | Ivory2 => "Ivory2"
          | Ivory3 => "Ivory3"
          | Ivory4 => "Ivory4"
          | Khaki1 => "Khaki1"
          | Khaki2 => "Khaki2"
          | Khaki3 => "Khaki3"
          | Khaki4 => "Khaki4"
          | Lavender => "Lavender"
          | Lavenderblush1 => "Lavenderblush1"
          | Lavenderblush2 => "Lavenderblush2"
          | Lavenderblush3 => "Lavenderblush3"
          | Lavenderblush4 => "Lavenderblush4"
          | Lawngreen => "Lawngreen"
          | Lemonchi => "Lemonchi"
          | Lightblue1 => "Lightblue1"
          | Lightblue2 => "Lightblue2"
          | Lightblue3 => "Lightblue3"
          | Lightblue4 => "Lightblue4"
          | Lightcyan1 => "Lightcyan1"
          | Lightcyan2 => "Lightcyan2"
          | Lightcyan3 => "Lightcyan3"
          | Lightcyan4 => "Lightcyan4"
          | Lightgoldenrod1 => "Lightgoldenrod1"
          | Lightgoldenrod2 => "Lightgoldenrod2"
          | Lightgoldenrod3 => "Lightgoldenrod3"
          | Lightgoldenrod4 => "Lightgoldenrod4"
          | Lightgoldenrodyellow => "Lightgoldenrodyellow"
          | Lightgray => "Lightgray"
          | Lightpink1 => "Lightpink1"
          | Lightpink2 => "Lightpink2"
          | Lightpink3 => "Lightpink3"
          | Lightpink4 => "Lightpink4"
          | Lightsalmon1 => "Lightsalmon1"
          | Lightsalmon2 => "Lightsalmon2"
          | Lightsalmon3 => "Lightsalmon3"
          | Lightsalmon4 => "Lightsalmon4"
          | Lightseagreen => "Lightseagreen"
          | Lightskyblue1 => "Lightskyblue1"
          | Lightskyblue2 => "Lightskyblue2"
          | Lightskyblue3 => "Lightskyblue3"
          | Lightskyblue4 => "Lightskyblue4"
          | Lightslateblue1 => "Lightslateblue1"
          | Lightslateblue2 => "Lightslateblue2"
          | Lightslateblue3 => "Lightslateblue3"
          | Lightslateblue4 => "Lightslateblue4"
          | Lightslategray => "Lightslategray"
          | Lightyellow1 => "Lightyellow1"
          | Lightyellow2 => "Lightyellow2"
          | Lightyellow3 => "Lightyellow3"
          | Lightyellow4 => "Lightyellow4"
          | Limegreen => "Limegreen"
          | Linen => "Linen"
          | Magenta1 => "Magenta1"
          | Magenta2 => "Magenta2"
          | Magenta3 => "Magenta3"
          | Magenta4 => "Magenta4"
          | Maroon1 => "Maroon1"
          | Maroon2 => "Maroon2"
          | Maroon3 => "Maroon3"
          | Maroon4 => "Maroon4"
          | Mediumaquamarine => "Mediumaquamarine"
          | Mediumblue => "Mediumblue"
          | Mediumorchid1 => "Mediumorchid1"
          | Mediumorchid2 => "Mediumorchid2"
          | Mediumorchid3 => "Mediumorchid3"
          | Mediumorchid4 => "Mediumorchid4"
          | Mediumpurple1 => "Mediumpurple1"
          | Mediumpurple2 => "Mediumpurple2"
          | Mediumpurple3 => "Mediumpurple3"
          | Mediumpurple4 => "Mediumpurple4"
          | Mediumseagreen => "Mediumseagreen"
          | Mediumslateblue => "Mediumslateblue"
          | Mediumspringgreen => "Mediumspringgreen"
          | Mediumturquoise => "Mediumturquoise"
          | Mediumvioletred => "Mediumvioletred"
          | Midnightblue => "Midnightblue"
          | Mintcream => "Mintcream"
          | Mistyrose1 => "Mistyrose1"
          | Mistyrose2 => "Mistyrose2"
          | Mistyrose3 => "Mistyrose3"
          | Mistyrose4 => "Mistyrose4"
          | Moccasin => "Moccasin"
          | Navajowhite1 => "Navajowhite1"
          | Navajowhite2 => "Navajowhite2"
          | Navajowhite3 => "Navajowhite3"
          | Navajowhite4 => "Navajowhite4"
          | Navy => "Navy"
          | Navyblue => "Navyblue"
          | Oldlace => "Oldlace"
          | Olivedrab1 => "Olivedrab1"
          | Olivedrab2 => "Olivedrab2"
          | Olivedrab3 => "Olivedrab3"
          | Olivedrab4 => "Olivedrab4"
          | On1 => "On1"
          | On2 => "On2"
          | On3 => "On3"
          | On4 => "On4"
          | Oralwhite => "Oralwhite"
          | Orange1 => "Orange1"
          | Orange2 => "Orange2"
          | Orange3 => "Orange3"
          | Orange4 => "Orange4"
          | Orangered1 => "Orangered1"
          | Orangered2 => "Orangered2"
          | Orangered3 => "Orangered3"
          | Orangered4 => "Orangered4"
          | Orchid1 => "Orchid1"
          | Orchid2 => "Orchid2"
          | Orchid3 => "Orchid3"
          | Orchid4 => "Orchid4"
          | Owerblue => "Owerblue"
          | Palegoldenrod => "Palegoldenrod"
          | Palegreen1 => "Palegreen1"
          | Palegreen2 => "Palegreen2"
          | Palegreen3 => "Palegreen3"
          | Palegreen4 => "Palegreen4"
          | Paleturquoise1 => "Paleturquoise1"
          | Paleturquoise2 => "Paleturquoise2"
          | Paleturquoise3 => "Paleturquoise3"
          | Paleturquoise4 => "Paleturquoise4"
          | Palevioletred1 => "Palevioletred1"
          | Palevioletred2 => "Palevioletred2"
          | Palevioletred3 => "Palevioletred3"
          | Palevioletred4 => "Palevioletred4"
          | Papayawhip => "Papayawhip"
          | Peachpu1 => "Peachpu1"
          | Peachpu2 => "Peachpu2"
          | Peachpu3 => "Peachpu3"
          | Peachpu4 => "Peachpu4"
          | Peru => "Peru"
          | Pink1 => "Pink1"
          | Pink2 => "Pink2"
          | Pink3 => "Pink3"
          | Pink4 => "Pink4"
          | Plum1 => "Plum1"
          | Plum2 => "Plum2"
          | Plum3 => "Plum3"
          | Plum4 => "Plum4"
          | Powderblue => "Powderblue"
          | Purple1 => "Purple1"
          | Purple2 => "Purple2"
          | Purple3 => "Purple3"
          | Purple4 => "Purple4"
          | Rebrick1 => "Rebrick1"
          | Rebrick2 => "Rebrick2"
          | Rebrick3 => "Rebrick3"
          | Rebrick4 => "Rebrick4"
          | Red1 => "Red1"
          | Red2 => "Red2"
          | Red3 => "Red3"
          | Red4 => "Red4"
          | Rosybrown1 => "Rosybrown1"
          | Rosybrown2 => "Rosybrown2"
          | Rosybrown3 => "Rosybrown3"
          | Rosybrown4 => "Rosybrown4"
          | Royalblue1 => "Royalblue1"
          | Royalblue2 => "Royalblue2"
          | Royalblue3 => "Royalblue3"
          | Royalblue4 => "Royalblue4"
          | Saddlebrown => "Saddlebrown"
          | Salmon1 => "Salmon1"
          | Salmon2 => "Salmon2"
          | Salmon3 => "Salmon3"
          | Salmon4 => "Salmon4"
          | Sandybrown => "Sandybrown"
          | Seagreen1 => "Seagreen1"
          | Seagreen2 => "Seagreen2"
          | Seagreen3 => "Seagreen3"
          | Seagreen4 => "Seagreen4"
          | Seashell1 => "Seashell1"
          | Seashell2 => "Seashell2"
          | Seashell3 => "Seashell3"
          | Seashell4 => "Seashell4"
          | Sienna1 => "Sienna1"
          | Sienna2 => "Sienna2"
          | Sienna3 => "Sienna3"
          | Sienna4 => "Sienna4"
          | Skyblue1 => "Skyblue1"
          | Skyblue2 => "Skyblue2"
          | Skyblue3 => "Skyblue3"
          | Skyblue4 => "Skyblue4"
          | Slateblue1 => "Slateblue1"
          | Slateblue2 => "Slateblue2"
          | Slateblue3 => "Slateblue3"
          | Slateblue4 => "Slateblue4"
          | Slategray1 => "Slategray1"
          | Slategray2 => "Slategray2"
          | Slategray3 => "Slategray3"
          | Slategray4 => "Slategray4"
          | Snow1 => "Snow1"
          | Snow2 => "Snow2"
          | Snow3 => "Snow3"
          | Snow4 => "Snow4"
          | Springgreen1 => "Springgreen1"
          | Springgreen2 => "Springgreen2"
          | Springgreen3 => "Springgreen3"
          | Springgreen4 => "Springgreen4"
          | Steelblue1 => "Steelblue1"
          | Steelblue2 => "Steelblue2"
          | Steelblue3 => "Steelblue3"
          | Steelblue4 => "Steelblue4"
          | Tan1 => "Tan1"
          | Tan2 => "Tan2"
          | Tan3 => "Tan3"
          | Tan4 => "Tan4"
          | Thistle1 => "Thistle1"
          | Thistle2 => "Thistle2"
          | Thistle3 => "Thistle3"
          | Thistle4 => "Thistle4"
          | Tomato1 => "Tomato1"
          | Tomato2 => "Tomato2"
          | Tomato3 => "Tomato3"
          | Tomato4 => "Tomato4"
          | Turquoise1 => "Turquoise1"
          | Turquoise2 => "Turquoise2"
          | Turquoise3 => "Turquoise3"
          | Turquoise4 => "Turquoise4"
          | Violet => "Violet"
          | Violetred1 => "Violetred1"
          | Violetred2 => "Violetred2"
          | Violetred3 => "Violetred3"
          | Violetred4 => "Violetred4"
          | Wheat1 => "Wheat1"
          | Wheat2 => "Wheat2"
          | Wheat3 => "Wheat3"
          | Wheat4 => "Wheat4"
          | White => "White"
          | Whitesmoke => "Whitesmoke"
          | Yellow1 => "Yellow1"
          | Yellow2 => "Yellow2"
          | Yellow3 => "Yellow3"
          | Yellow4 => "Yellow4"
          | Yellowgreen => "Yellowgreen"
   end
