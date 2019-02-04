{-# LANGUAGE OverloadedStrings #-}

module Templates
    (
      -- Templates
      tempBool
    , tempNil
    , tempLit
    , tempStrings
    , tempDict1
    , tempDict2
    , tempEmpty
    , tempComment1
    , tempComment2
    , tempIf
    , tempUnless
    , tempWith
    , tempArith
    ) where

import qualified Data.ByteString.Lazy as Lbs

-- Template to test booleans.
tempBool :: Lbs.ByteString
tempBool = "This is {{ true }}. That is {{ false }}."

-- Template to test nil.
tempNil :: Lbs.ByteString
tempNil = "Nothing is here, except {{ nil }}."

-- Template to test literals.
tempLit :: Lbs.ByteString
tempLit = "First {{ 10 }} digits of {{ \"pi\" }} is {{ 3.1415926535 }}."

-- Template to test multiple strings.
tempStrings :: Lbs.ByteString
tempStrings = "{{ \"Hello\" }} {{ \"world\" }}"

-- Template to test dictionary literal.
tempDict1 :: Lbs.ByteString
tempDict1 = "There are {{ {\"apples\" 5 \"oranges\" 2} \"apples\" }} apples."

-- Template to test nested dictionary.
tempDict2 :: Lbs.ByteString
tempDict2 = "Earth weights {{ get-in $planets (\"earth\" \"weight\") }} kg."

-- Template to test empty template.
tempEmpty :: Lbs.ByteString
tempEmpty = ""

-- Template to test comment.
tempComment1 :: Lbs.ByteString
tempComment1 = "{{! I am invisible }}"

-- Template to test comment block in between texts.
tempComment2 :: Lbs.ByteString
tempComment2 = "hello {{! I am invisible }}world."

-- Template to test if block.
tempIf :: Lbs.ByteString
tempIf = "{{# if (defined? $name) }}Hello {{ $name }}.{{/#}}"

-- Template to test unless block.
tempUnless :: Lbs.ByteString
tempUnless = "{{# unless (defined? $name) }}Hello world.{{/#}}"

-- Template to test with block.
tempWith :: Lbs.ByteString
tempWith = "{{# with $list }}{{ $$element }} and {{/#}}"

-- Template to test arithmetic.
tempArith :: Lbs.ByteString
tempArith = "The answer is {{ + (- 50 20) 12 }}."
