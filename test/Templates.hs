module Templates
    (
      -- Templates
      tempBool
    , tempNil
    , tempLit
    , tempComment1
    , tempComment2
    , tempIf
    , tempUnless
    , tempWith
    , tempArith
    ) where

-- Template to test booleans.
tempBool :: String
tempBool = "This is {{ true }}. That is {{ false }}."

-- Template to test nil.
tempNil :: String
tempNil = "Nothing is here, except {{ nil }}."

-- Template to test literals.
tempLit :: String
tempLit = "First {{ 10 }} digits of {{ \"pi\" }} is {{ 3.1415926535 }}."

-- Template to test comment.
tempComment1 :: String
tempComment1 = "{{! I am invisible }}"

-- Template to test comment block in between texts.
tempComment2 :: String
tempComment2 = "hello {{! I am invisible }}world."

-- Template to test if block.
tempIf :: String
tempIf = "{{# if (defined? $name) }}Hello {{ $name }}.{{/#}}"

-- Template to test unless block.
tempUnless :: String
tempUnless = "{{# unless (defined? $name) }}Hello world.{{/#}}"

-- Template to test with block.
tempWith :: String
tempWith = "{{# with $list }}{{ $$element }} and {{/#}}"

-- Template to test arithmetic.
tempArith :: String
tempArith = "The answer is {{ + (- 50 20) 12 }}."
