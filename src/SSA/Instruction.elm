module SSA.Instruction exposing (..)

import SSA.Types exposing (..)

{-

Instruction
-------

<Describe me if possible...>

-}


empty : InstructionList
empty = []


single : Instruction -> InstructionList
single i = [ i ]


