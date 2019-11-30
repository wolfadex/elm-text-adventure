module Color exposing (ColorSet, dark, light)

import Element exposing (Color)


type alias ColorSet =
    { background : Color
    , font : Color
    , border : Color
    , button : Color
    }


light : ColorSet
light =
    { background = Element.rgb255 213 204 186
    , font = Element.rgb255 69 55 60
    , border = Element.rgb255 151 82 44
    , button = Element.rgb255 234 165 73
    }


dark : ColorSet
dark =
    { background = Element.rgb255 32 17 27
    , font = Element.rgb255 150 140 131
    , border = Element.rgb255 151 82 44
    , button = Element.rgb255 66 106 121
    }
